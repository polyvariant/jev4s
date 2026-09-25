package jev4s

import cats.data.NonEmptyVector
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import jev4s.internal.OptionsMacros

/** The possible answers of a Choice, or the ordered levels of a Score.
  *
  * For a Choice, `label` is the option key sent to the model (it carries meaning, unlike question IDs). For a
  * Score, `values` must be ordered from lowest to highest level.
  */
trait Options[A] {
  def values: NonEmptyVector[A]
  def label(a: A): String
  def description(a: A): Option[Json]

  final def labelledBy(f: A => String): Options[A] = Options.instance(values, f, description)

  final def describedBy[D: Encoder](f: A => D): Options[A] = Options.instance(values, label, a => Some(f(a).asJson))
}

object Options {

  def apply[A](using o: Options[A]): Options[A] = o

  def instance[A](values: NonEmptyVector[A], label: A => String, description: A => Option[Json]): Options[A] = {
    val (v, l, d) = (values, label, description)
    new Options[A] {
      val values: NonEmptyVector[A] = v
      def label(a: A): String = l(a)
      def description(a: A): Option[Json] = d(a)
    }
  }

  /** Ad-hoc options with no descriptions. */
  def labels(first: String, rest: String*): Options[String] =
    instance(NonEmptyVector(first, rest.toVector), identity, _ => None)

  /** Ad-hoc options with a description each. */
  def described[D: Encoder](first: (String, D), rest: (String, D)*): Options[String] = {
    val all = NonEmptyVector(first, rest.toVector)
    val descriptions = all.toVector.toMap
    instance(all.map(_._1), identity, l => descriptions.get(l).map(_.asJson))
  }

  /** Derives options for an enum (or sealed trait) of parameterless cases, in declaration order.
    *
    * Each case is labelled by its name unless annotated with [[label]], and described by its [[description]]
    * annotation if present. Also usable as `derives Options`.
    */
  inline def derived[A]: Options[A] = ${ OptionsMacros.derive[A] }

  /** Used by [[derived]]'s expansion: (value, label, description) per case. */
  def fromCases[A](cases: (A, String, Option[String])*): Options[A] = {
    val all = NonEmptyVector.fromVectorUnsafe(cases.toVector)
    val labels = all.toVector.map((a, l, _) => a -> l).toMap
    val descriptions = all.toVector.map((a, _, d) => a -> d).toMap
    instance(all.map(_._1), labels, descriptions(_).map(_.asJson))
  }

  /** Score levels given by position: level `i` is described by the `i`-th string. */
  def levels(first: String, second: String, rest: String*): Options[Int] = {
    val all = first +: second +: rest.toVector
    instance(NonEmptyVector.fromVectorUnsafe(all.indices.toVector), all(_), i => Some(all(i).asJson))
  }
}
