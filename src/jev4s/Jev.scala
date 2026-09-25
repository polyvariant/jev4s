package jev4s

import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Env
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import jev4s.internal.ModelList
import jev4s.internal.ResponseBody
import org.http4s.EntityDecoder
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import org.http4s.AuthScheme
import org.http4s.Credentials

import scala.concurrent.duration.*

/** A client for TypeSafe's System One models (Jev). */
trait Jev[F[_]] {

  /** Evaluates every question in `question` against `state` in one request. `state` is any JSON-encodable value:
    * a String for plain text, or a case class / map / list for structured context.
    */
  def evaluate[S: Encoder, A](state: S, question: Question[A]): F[Evaluation[A]]

  def models: F[List[ModelCard]]

  /** The same client, sending `model` instead of the configured one. */
  def withModel(model: ModelId): Jev[F]
}

final case class JevConfig(
  apiKey: ApiKey,
  baseUri: Uri = uri"https://api.typesafe.ai",
  model: ModelId = ModelId.latest,
  timeout: FiniteDuration = 10.seconds,
  retry: JevConfig.RetryConfig = JevConfig.RetryConfig.default,
)

object JevConfig {

  final case class RetryConfig(maxRetries: Int, maxBackoff: FiniteDuration)

  object RetryConfig {
    val default: RetryConfig = RetryConfig(maxRetries = 2, maxBackoff = 5.seconds)
    val disabled: RetryConfig = RetryConfig(maxRetries = 0, maxBackoff = Duration.Zero)
  }

  /** Reads `TYPESAFE_API_KEY` (required), `TYPESAFE_BASE_URL` and `TYPESAFE_DEFAULT_MODEL`, like the official SDKs. */
  def fromEnv[F[_]: Env: Concurrent]: F[JevConfig] =
    (
      Env[F].get("TYPESAFE_API_KEY").flatMap(_.liftTo[F](JevError.MissingApiKey)),
      Env[F].get("TYPESAFE_BASE_URL").flatMap(_.traverse(Uri.fromString(_).liftTo[F])),
      Env[F].get("TYPESAFE_DEFAULT_MODEL"),
    ).mapN { (key, baseUri, model) =>
      val base = JevConfig(ApiKey(key))
      base.copy(
        baseUri = baseUri.getOrElse(base.baseUri),
        model = model.fold(base.model)(ModelId(_)),
      )
    }
}

enum JevError(message: String) extends Exception(message) {
  case MissingApiKey extends JevError("TYPESAFE_API_KEY is not set")
  case InvalidQuestion(reason: String) extends JevError(s"Invalid question: $reason")
  case Unauthorized(body: String) extends JevError(s"Unauthorized: $body")
  case Unprocessable(body: Json) extends JevError(s"Request failed validation: ${body.noSpaces}")
  case RateLimited(body: String) extends JevError(s"Rate limited: $body")
  case Overloaded(body: String) extends JevError(s"Overloaded: $body")
  case UnexpectedStatus(status: Status, body: String) extends JevError(s"Unexpected status $status: $body")
  case UnexpectedAnswer(reason: String) extends JevError(s"Unexpected answer: $reason")
}

object Jev {

  def instance[F[_]: Temporal](config: JevConfig)(using client: Client[F]): Jev[F] =
    JevImpl(config, withRetries(config.retry, client))

  /** Builds an Ember client with the configured per-attempt timeout. */
  def resource[F[_]: Async: Network](config: JevConfig): Resource[F, Jev[F]] =
    EmberClientBuilder.default[F].withTimeout(config.timeout).build.map { client =>
      given Client[F] = client
      instance(config)
    }

  // Same statuses as the official SDKs: 408, 429, 5xx (529 included). Retry-After is honored by the middleware.
  private def withRetries[F[_]: Temporal](config: JevConfig.RetryConfig, client: Client[F]): Client[F] =
    if (config.maxRetries <= 0) client
    else
      Retry[F](
        RetryPolicy(
          RetryPolicy.exponentialBackoff(config.maxBackoff, config.maxRetries),
          (_, result) =>
            result.fold(_ => true, r => r.status.code == 408 || r.status.code == 429 || r.status.code >= 500),
        ),
        Headers.SensitiveHeaders.contains,
      )(client)

  private final class JevImpl[F[_]: Concurrent] private[Jev] (config: JevConfig, client: Client[F])
    extends Jev[F] {

    def evaluate[S: Encoder, A](state: S, question: Question[A]): F[Evaluation[A]] =
      for {
        body <- Question.requestBody(state, question, config.model).leftMap(JevError.InvalidQuestion(_)).liftTo[F]
        response <- client.run(authorized(Method.POST, "v1/systemone").withEntity(body)).use(decodeOrFail[ResponseBody])
        evaluation <- Question.decodeResponse(question, response).leftMap(JevError.UnexpectedAnswer(_)).liftTo[F]
      } yield evaluation

    def models: F[List[ModelCard]] =
      client
        .run(authorized(Method.GET, "v1/models"))
        .use(decodeOrFail[ModelList])
        .map(_.models)

    def withModel(model: ModelId): Jev[F] = JevImpl(config.copy(model = model), client)

    private def authorized(method: Method, path: String): Request[F] =
      Request[F](method, config.baseUri.addPath(path))
        .putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, config.apiKey.value)))

    private def decodeOrFail[A: Decoder](response: Response[F]): F[A] = {
      given EntityDecoder[F, A] = jsonOf[F, A]
      response.status match {
        case s if s.isSuccess                => response.as[A]
        case Status.Unauthorized             => response.as[String].flatMap(JevError.Unauthorized(_).raiseError)
        case Status.UnprocessableContent     => response.as[Json].flatMap(JevError.Unprocessable(_).raiseError)
        case Status.TooManyRequests          => response.as[String].flatMap(JevError.RateLimited(_).raiseError)
        case s if s.code == 529              => response.as[String].flatMap(JevError.Overloaded(_).raiseError)
        case s                               => response.as[String].flatMap(JevError.UnexpectedStatus(s, _).raiseError)
      }
    }
  }
}
