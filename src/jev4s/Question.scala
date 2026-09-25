package jev4s

import cats.Applicative
import cats.~>
import cats.data.StateT
import cats.free.FreeApplicative
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import jev4s.internal.NoulCriteria
import jev4s.internal.QuestionSpec
import jev4s.internal.RawAnswer
import jev4s.internal.ResponseBody

import scala.collection.immutable.ListMap

/** Answer to a yes/no question. There is no separate confidence: 0.5 means "yes and no equally likely". */
final case class Noul(yes: Probability) {
  def no: Probability = yes.complement
}

final case class Choice[A](choice: A, probabilities: Map[A, Probability], confidence: Confidence)

/** `score` is the probability-weighted level index, and can land between levels. */
final case class Score[A](score: Double, probabilities: Map[A, Probability], confidence: Confidence) {
  def mostLikely: A = probabilities.maxBy(_._2)._1
}

/** A single question: what goes over the wire, and how to read its answer. Lifted into [[Question]]. */
private[jev4s] final case class Ask[A](
  spec: QuestionSpec,
  decode: RawAnswer => Either[String, A],
  // Definition errors, reported by `evaluate` before anything is sent.
  problems: Vector[String],
)

/** One or more questions about the same state, whose combined answer is an `A`.
  *
  * Compose with `mapN`/`tupled`/`traverse`: all questions in a `Question` go out in a single request and are
  * evaluated in parallel, independently of each other. Question IDs are assigned by the library (the model never
  * sees them), so answers can't be looked up under the wrong key.
  */
opaque type Question[A] = FreeApplicative[Ask, A]

object Question {

  given Applicative[Question] = summon[Applicative[FreeApplicative[Ask, *]]]

  def noul[I: Encoder](instructions: I): Question[Noul] =
    single(QuestionSpec.Noul(instructions.asJson, None)) { case RawAnswer.Noul(p) => Right(Noul(p)) }

  /** A Noul with descriptions of what "yes" and "no" mean. */
  def noul[I: Encoder, Y: Encoder, N: Encoder](instructions: I, yes: Y, no: N): Question[Noul] =
    single(QuestionSpec.Noul(instructions.asJson, Some(NoulCriteria(Some(yes.asJson), Some(no.asJson))))) {
      case RawAnswer.Noul(p) => Right(Noul(p))
    }

  /** Picks one of `A`'s options, e.g. the cases of an enum with a `given Options`. */
  def choice[A](using options: Options[A]): ChoicePartiallyApplied[A] = ChoicePartiallyApplied(options)

  /** Rates the state on `A`'s levels, lowest first. */
  def score[A](using levels: Options[A]): ScorePartiallyApplied[A] = ScorePartiallyApplied(levels)

  final class ChoicePartiallyApplied[A] private[Question] (options: Options[A]) {
    def apply[I: Encoder](instructions: I): Question[Choice[A]] = {
      val labels = options.values.toVector.map(options.label)
      val byLabel = labels.zip(options.values.toVector).toMap
      val spec = QuestionSpec.Choice(
        instructions.asJson,
        ListMap.from(options.values.toVector.map(a => options.label(a) -> options.description(a))),
      )
      val duplicates = labels.diff(labels.distinct).distinct
      val problems = Option.when(duplicates.nonEmpty)(s"Duplicate Choice labels: ${duplicates.mkString(", ")}")
      single(spec, problems.toVector) { case RawAnswer.Choice(choice, probabilities, confidence) =>
        (
          lookup(byLabel, choice),
          probabilities.toList.traverse((k, p) => lookup(byLabel, k).tupleRight(p)).map(_.toMap),
        ).mapN(Choice(_, _, confidence))
      }
    }
  }

  final class ScorePartiallyApplied[A] private[Question] (levels: Options[A]) {
    def apply[I: Encoder](instructions: I): Question[Score[A]] = {
      val byIndex = levels.values.toVector.zipWithIndex.map((a, i) => i.toString -> a).toMap
      // Score criteria are descriptions only; fall back to the label when a level has none.
      val spec = QuestionSpec.Score(
        instructions.asJson,
        levels.values.toVector.map(a => levels.description(a).getOrElse(levels.label(a).asJson)),
      )
      single(spec) { case RawAnswer.Score(score, probabilities, confidence) =>
        probabilities.toList
          .traverse((k, p) => lookup(byIndex, k).tupleRight(p))
          .map(ps => Score(score, ps.toMap, confidence))
      }
    }
  }

  private def lookup[A](m: Map[String, A], key: String): Either[String, A] =
    m.get(key).toRight(s"Unexpected option in answer: $key")

  private def single[A](spec: QuestionSpec, problems: Vector[String] = Vector.empty)(
    f: PartialFunction[RawAnswer, Either[String, A]]
  ): Question[A] =
    FreeApplicative.lift(
      Ask(spec, raw => f.applyOrElse(raw, other => Left(s"Answer type mismatch: expected $spec, got $other")), problems)
    )

  private def idOf(index: Int): String = s"q$index"

  private def specs[A](question: Question[A]): Vector[QuestionSpec] =
    question.analyze(new (Ask ~> ([x] =>> Vector[QuestionSpec])) {
      def apply[x](ask: Ask[x]): Vector[QuestionSpec] = Vector(ask.spec)
    })

  private def problems[A](question: Question[A]): Vector[String] =
    question.analyze(new (Ask ~> ([x] =>> Vector[String])) {
      def apply[x](ask: Ask[x]): Vector[String] = ask.problems
    })

  private[jev4s] def requestBody[S: Encoder, A](state: S, question: Question[A], model: ModelId): Either[String, Json] = {
    val all = specs(question)
    val definitionProblems = problems(question)
    for {
      _ <- Either.cond(all.nonEmpty, (), "At least one question is required")
      _ <- Either.cond(definitionProblems.isEmpty, (), definitionProblems.mkString("; "))
      _ <- all.traverse_(_.validate)
    } yield Json.obj(
      "state" := state,
      "model" := model,
      "questions" := JsonObject.fromIterable(all.zipWithIndex.map((s, i) => idOf(i) -> s.asJson)),
    )
  }

  // Walks the questions in the same order as `specs`, so the n-th question reads the answer under `idOf(n)`.
  private type Decoding[x] = StateT[Either[String, *], Int, x]

  private[jev4s] def decodeResponse[A](question: Question[A], body: ResponseBody): Either[String, Evaluation[A]] =
    question
      .foldMap(new (Ask ~> Decoding) {
        def apply[x](ask: Ask[x]): Decoding[x] = StateT { index =>
          body.answers
            .get(idOf(index))
            .toRight(s"Missing answer for question ${idOf(index)}")
            .flatMap(ask.decode)
            .tupleLeft(index + 1)
        }
      })
      .runA(0)
      .map(Evaluation(_, body.model, body.usage))
}
