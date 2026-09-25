package jev4s

import cats.Order
import io.circe.Decoder
import io.circe.Encoder
import hearth.kindlings.circederivation.KindlingsDecoder
import jev4s.internal.wireConfig

/** A probability in [0, 1]. For a Noul, the probability that the answer is "yes". */
opaque type Probability = Double

object Probability {

  def apply(value: Double): Either[String, Probability] =
    Either.cond(value >= 0 && value <= 1, value, s"Probability out of range: $value")

  // The API sends floats; tolerate rounding noise at the edges instead of failing the whole response.
  private[jev4s] def clamped(value: Double): Probability = math.max(0.0, math.min(1.0, value))

  extension (p: Probability) {
    def value: Double = p
    def complement: Probability = 1.0 - p
  }

  given Order[Probability] = Order.fromOrdering(using Ordering.Double.TotalOrdering)
  given Ordering[Probability] = Ordering.Double.TotalOrdering
  given Decoder[Probability] = Decoder.decodeDouble.map(clamped)
}

/** How concentrated a Choice/Score distribution is, in [0, 1]. Not a measure of correctness. */
opaque type Confidence = Double

object Confidence {

  def apply(value: Double): Either[String, Confidence] =
    Either.cond(value >= 0 && value <= 1, value, s"Confidence out of range: $value")

  extension (c: Confidence) {
    def value: Double = c
  }

  given Order[Confidence] = Order.fromOrdering(using Ordering.Double.TotalOrdering)
  given Ordering[Confidence] = Ordering.Double.TotalOrdering
  given Decoder[Confidence] = Decoder.decodeDouble.map(d => math.max(0.0, math.min(1.0, d)))
}

/** A model name or alias, e.g. `jev-latest` or a pinned `jev-1.13.0`. */
opaque type ModelId = String

object ModelId {
  def apply(value: String): ModelId = value

  /** The most recent stable release. Moves when a new release ships. */
  val latest: ModelId = "jev-latest"

  /** The most recent release, official or not. */
  val preview: ModelId = "jev-preview"

  extension (m: ModelId) {
    def value: String = m
  }

  given Encoder[ModelId] = Encoder.encodeString
  given Decoder[ModelId] = Decoder.decodeString
}

final case class Usage(inputTokens: Long, outputTokens: Long)

object Usage {
  given Decoder[Usage] = KindlingsDecoder.derived(using wireConfig)
}

final case class ModelCard(name: ModelId, description: String, releaseDate: String)

object ModelCard {
  given Decoder[ModelCard] = KindlingsDecoder.derived(using wireConfig)
}

/** The result of one evaluation: your typed answers plus request metadata. */
final case class Evaluation[+A](answers: A, model: ModelId, usage: Usage) {
  def map[B](f: A => B): Evaluation[B] = copy(answers = f(answers))
}

final case class ApiKey(value: String) {
  override def toString: String = "ApiKey(<redacted>)"
}
