/*
 * Copyright 2026 Polyvariant
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jev4s.internal

import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.circederivation.KindlingsDecoder
import hearth.kindlings.circederivation.KindlingsEncoder
import hearth.kindlings.circederivation.annotations.fieldName
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import jev4s.Confidence
import jev4s.ModelCard
import jev4s.ModelId
import jev4s.Probability
import jev4s.Usage

import scala.collection.immutable.ListMap

/** The API's JSON conventions: snake_case fields, and a lowercase `type` discriminator on
  * questions/answers.
  */
private[jev4s] val wireConfig: Configuration =
  Configuration()
    .withSnakeCaseMemberNames
    .withDiscriminator("type")
    .withTransformConstructorNames(_.toLowerCase)

/** One question as it goes over the wire, minus its ID. */
private[jev4s] enum QuestionSpec {
  case Noul(instructions: Json, criteria: Option[NoulCriteria])
  case Choice(instructions: Json, criteria: ListMap[String, Option[Json]])
  case Score(instructions: Json, criteria: Vector[Json])

  def validate: Either[String, Unit] =
    this match {
      case Noul(_, _)         => Right(())
      case Choice(_, options) =>
        Either.cond(
          options.sizeIs <= 255,
          (),
          s"A Choice accepts at most 255 options, got ${options.size}",
        )
      case Score(_, levels) =>
        Either.cond(
          levels.sizeIs >= 2 && levels.sizeIs <= 10,
          (),
          s"A Score needs 2 to 10 levels, got ${levels.size}",
        )
    }

}

private[jev4s] object QuestionSpec {

  // An absent Noul `criteria` is omitted rather than sent as null: Clef rejects the null. Only the
  // top level is filtered, since null is a valid Choice option description.
  given Encoder[QuestionSpec] = KindlingsEncoder
    .derived[QuestionSpec](
      using wireConfig
    )
    .mapJson(_.mapObject(_.filter((_, v) => !v.isNull)))

}

private[jev4s] final case class NoulCriteria(
  @fieldName("true") yes: Option[Json],
  @fieldName("false") no: Option[Json],
)

private[jev4s] enum RawAnswer {
  case Noul(noul: Probability)
  case Choice(choice: String, probabilities: Map[String, Probability], confidence: Confidence)
  case Score(score: Double, probabilities: Map[String, Probability], confidence: Confidence)
}

private[jev4s] object RawAnswer {

  given Decoder[RawAnswer] = KindlingsDecoder.derived(
    using wireConfig
  )

}

private[jev4s] final case class ResponseBody(
  model: ModelId,
  answers: Map[String, RawAnswer],
  usage: Usage,
)

private[jev4s] object ResponseBody {

  given Decoder[ResponseBody] = KindlingsDecoder.derived(
    using wireConfig
  )

}

private[jev4s] final case class ModelList(models: List[ModelCard])

private[jev4s] object ModelList {

  given Decoder[ModelList] = KindlingsDecoder.derived(
    using wireConfig
  )

}
