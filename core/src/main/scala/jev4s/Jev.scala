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

package jev4s

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import jev4s.internal.ModelList
import jev4s.internal.ResponseBody
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy

import scala.concurrent.duration.*

/** A client for TypeSafe's System One models (Jev). */
trait Jev[F[_]] {

  /** Evaluates every question in `question` against `state` in one request. `state` is any
    * JSON-encodable value: a String for plain text, or a case class / map / list for structured
    * context.
    */
  def evaluate[S: Encoder, A](state: S, question: Question[A]): F[Evaluation[A]]

  def models: F[List[ModelCard]]

  /** The same client, sending `model` instead of the provider's default. */
  def withModel(model: ModelId): Jev[F]
}

final case class JevConfig[F[_]](
  provider: Provider[F],
  retry: JevConfig.RetryConfig = JevConfig.RetryConfig.default,
)

object JevConfig {

  final case class RetryConfig(maxRetries: Int, maxBackoff: FiniteDuration)

  object RetryConfig {
    val default: RetryConfig = RetryConfig(maxRetries = 2, maxBackoff = 5.seconds)
    val disabled: RetryConfig = RetryConfig(maxRetries = 0, maxBackoff = Duration.Zero)
  }

}

enum JevError(message: String) extends Exception(message) {
  case MissingEnv(name: String) extends JevError(s"$name is not set")
  case Unsupported(operation: String)
    extends JevError(s"Not supported by this provider: $operation")
  case InvalidQuestion(reason: String) extends JevError(s"Invalid question: $reason")
  case Unauthorized(body: String) extends JevError(s"Unauthorized: $body")
  case Unprocessable(body: Json) extends JevError(s"Request failed validation: ${body.noSpaces}")
  case RateLimited(body: String) extends JevError(s"Rate limited: $body")
  case Overloaded(body: String) extends JevError(s"Overloaded: $body")
  case UnexpectedStatus(status: Status, body: String)
    extends JevError(s"Unexpected status $status: $body")
  case UnexpectedAnswer(reason: String) extends JevError(s"Unexpected answer: $reason")
}

object Jev {

  /** Timeouts, connection pooling etc. are up to the `Client` you provide; retries are added on top
    * of it.
    */
  def instance[F[_]: Temporal](config: JevConfig[F], client: Client[F]): Jev[F] =
    JevImpl(
      config.provider,
      config.provider.defaultModel,
      withRetries(config.retry, authorized(config.provider, client)),
    )

  // Under the retries, so that every attempt asks the provider for credentials.
  private def authorized[F[_]: MonadCancelThrow](provider: Provider[F], client: Client[F])
    : Client[F] = Client(request => Resource.eval(provider.authorize(request)).flatMap(client.run))

  // Same statuses as the official SDKs: 408, 429, 5xx (529 included). Retry-After is honored by the middleware.
  private def withRetries[F[_]: Temporal](config: JevConfig.RetryConfig, client: Client[F])
    : Client[F] =
    if (config.maxRetries <= 0)
      client
    else
      Retry[F](
        RetryPolicy(
          RetryPolicy.exponentialBackoff(config.maxBackoff, config.maxRetries),
          (_, result) =>
            result.fold(
              _ => true,
              r => r.status.code == 408 || r.status.code == 429 || r.status.code >= 500,
            ),
        ),
        Headers.SensitiveHeaders.contains,
      )(client)

  private final class JevImpl[F[_]: Concurrent] private[Jev] (
    provider: Provider[F],
    model: ModelId,
    client: Client[F],
  ) extends Jev[F] {

    def evaluate[S: Encoder, A](state: S, question: Question[A]): F[Evaluation[A]] =
      for {
        body <- Question
          .requestBody(state, question, model)
          .leftMap(JevError.InvalidQuestion(_))
          .liftTo[F]
        response <- client
          .run(
            Request[F](Method.POST, provider.evaluateUri(model)).withEntity(body)
          )
          .use(decodeOrFail[ResponseBody])
        evaluation <- Question
          .decodeResponse(question, response)
          .leftMap(JevError.UnexpectedAnswer(_))
          .liftTo[F]
      } yield evaluation

    def models: F[List[ModelCard]] =
      provider.modelsUri match {
        case Some(uri) =>
          client
            .run(Request[F](Method.GET, uri))
            .use(decodeOrFail[ModelList])
            .map(_.models)
        case None => JevError.Unsupported("listing models").raiseError
      }

    def withModel(model: ModelId): Jev[F] = JevImpl(provider, model, client)

    private def decodeOrFail[A: Decoder](response: Response[F]): F[A] =
      response.status match {
        case s if s.isSuccess =>
          response
            .as[Json]
            .flatMap(
              provider
                .unwrap(_)
                .flatMap(_.as[A].leftMap(_.getMessage))
                .leftMap(JevError.UnexpectedAnswer(_))
                .liftTo[F]
            )
        case Status.Unauthorized => response.as[String].flatMap(JevError.Unauthorized(_).raiseError)
        case Status.UnprocessableContent =>
          response.as[Json].flatMap(JevError.Unprocessable(_).raiseError)
        case Status.TooManyRequests =>
          response.as[String].flatMap(JevError.RateLimited(_).raiseError)
        case s if s.code == 529 => response.as[String].flatMap(JevError.Overloaded(_).raiseError)
        case s => response.as[String].flatMap(JevError.UnexpectedStatus(s, _).raiseError)
      }

  }

}
