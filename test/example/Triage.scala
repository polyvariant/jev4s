package example

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import io.circe.Encoder
import jev4s.*

enum Department derives Options {
  @label("billing") @description("Payments, invoicing, refunds") case Billing
  @label("technical") @description("Bugs, outages, integrations") case Technical
  @label("sales") @description("Pricing, upgrades, new accounts") case Sales
}

enum Frustration derives Options {
  @description("Calm") case Calm
  @description("Frustrated") case Frustrated
  @description("Very angry") case VeryAngry
}

final case class Ticket(subject: String, message: String) derives Encoder.AsObject

final case class Triage(
  urgent: Noul,
  department: Choice[Department],
  frustration: Score[Frustration],
  // One Noul per label: several can apply at once, so this isn't a Choice.
  topics: Map[String, Noul],
)

object Triage {

  val topics: List[String] = List("refund", "account access", "data loss")

  val question: Question[Triage] = (
    Question.noul(
      "Does `message` convey urgency?",
      yes = "Explicitly time-sensitive",
      no = "No urgency expressed",
    ),
    Question.choice[Department]("Which team should handle this ticket?"),
    Question.score[Frustration]("How frustrated is the customer?"),
    topics.traverse(t => Question.noul(s"Does the ticket ask about $t?").tupleLeft(t)).map(_.toMap),
  ).mapN(Triage.apply)
}

object Main extends IOApp.Simple {

  val run: IO[Unit] =
    JevConfig.fromEnv[IO].toResource.flatMap(Jev.resource[IO]).use { jev =>
      for {
        result <- jev.evaluate(
          Ticket("Payouts", "Help! My payouts have been failing for 3 days."),
          Triage.question,
        )
        triage = result.answers
        _ <- IO.println(s"answered by ${result.model.value}, ${result.usage.inputTokens} input tokens")
        _ <- IO.println(s"urgent: ${triage.urgent.yes.value}")
        _ <- IO.println(s"department: ${triage.department.choice} (confidence ${triage.department.confidence.value})")
        _ <- IO.println(s"frustration: ${triage.frustration.score} ~ ${triage.frustration.mostLikely}")
        _ <- IO.println(s"topics: ${triage.topics.filter(_._2.yes.value > 0.5).keys.mkString(", ")}")
      } yield ()
    }
}
