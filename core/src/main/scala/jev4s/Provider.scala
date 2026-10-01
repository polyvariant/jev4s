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

import cats.Applicative
import cats.MonadThrow
import cats.effect.std.Env
import cats.syntax.all.*
import io.circe.Json
import org.http4s.AuthScheme
import org.http4s.Credentials
import org.http4s.Request
import org.http4s.Uri
import org.http4s.headers.Authorization
import org.http4s.implicits.*

/** An API that serves the System One format. Requests and answers look the same everywhere, but
  * endpoints and credentials differ: implement this to point jev4s at a new one.
  */
trait Provider[F[_]] {

  /** The model to use unless `Jev.withModel` says otherwise. */
  def defaultModel: ModelId

  /** Where evaluations with `model` are POSTed. */
  def evaluateUri(model: ModelId): Uri

  /** Where the model list is fetched from, if the API has one. */
  def modelsUri: Option[Uri]

  /** Adds credentials to a request. Runs before every request, so it can fetch or refresh them. */
  def authorize(request: Request[F]): F[Request[F]]

  /** Finds the System One response in a successful response body, e.g. inside an envelope. */
  def unwrap(body: Json): Either[String, Json] = Right(body)
}

object Provider {

  private val typeSafeUri = uri"https://api.typesafe.ai"

  /** TypeSafe's own API. */
  def typeSafe[F[_]: Applicative](
    apiKey: ApiKey,
    baseUri: Uri = typeSafeUri,
    defaultModel: ModelId = ModelId.latest,
  ): Provider[F] = {
    val model = defaultModel
    new Provider[F] {
      def defaultModel: ModelId = model
      def evaluateUri(model: ModelId): Uri = baseUri / "v1" / "systemone"
      def modelsUri: Option[Uri] = Some(baseUri / "v1" / "models")
      def authorize(request: Request[F]): F[Request[F]] = bearer(request, apiKey).pure[F]
    }
  }

  /** Reads `TYPESAFE_API_KEY` (required), `TYPESAFE_BASE_URL` and `TYPESAFE_DEFAULT_MODEL`, like
    * the official SDKs. They're read once, not on every request.
    */
  def typeSafeFromEnv[F[_]: Env: MonadThrow]: F[Provider[F]] =
    (
      required[F]("TYPESAFE_API_KEY"),
      Env[F].get("TYPESAFE_BASE_URL").flatMap(_.traverse(Uri.fromString(_).liftTo[F])),
      Env[F].get("TYPESAFE_DEFAULT_MODEL"),
    ).mapN { (key, baseUri, model) =>
      typeSafe[F](
        ApiKey(key),
        baseUri = baseUri.getOrElse(typeSafeUri),
        defaultModel = model.fold(ModelId.latest)(ModelId(_)),
      )
    }

  /** Cloudflare Workers AI, serving the Clef models. `apiToken` needs the "Workers AI - Read" and
    * "Workers AI - Edit" permissions. There's no model list, and answers come wrapped in
    * Cloudflare's `{"result": ..., "success": ...}` envelope.
    */
  def workersAI[F[_]: Applicative](
    accountId: String,
    apiToken: ApiKey,
    baseUri: Uri = uri"https://api.cloudflare.com/client/v4",
    defaultModel: ModelId = ModelId.clef,
  ): Provider[F] = {
    val model = defaultModel
    new Provider[F] {
      def defaultModel: ModelId = model

      def evaluateUri(model: ModelId): Uri =
        baseUri / "accounts" / accountId / "ai" / "run" / "@cf" / "cloudflare" / model.value

      def modelsUri: Option[Uri] = None
      def authorize(request: Request[F]): F[Request[F]] = bearer(request, apiToken).pure[F]

      override def unwrap(body: Json): Either[String, Json] =
        body.hcursor.downField("result").focus.toRight("Missing `result` in the response")
    }
  }

  /** Reads `CLOUDFLARE_ACCOUNT_ID` and `CLOUDFLARE_AUTH_TOKEN` (both required) and
    * `CLOUDFLARE_MODEL` (`clef` by default). They're read once, not on every request.
    */
  def workersAIFromEnv[F[_]: Env: MonadThrow]: F[Provider[F]] =
    (
      required[F]("CLOUDFLARE_ACCOUNT_ID"),
      required[F]("CLOUDFLARE_AUTH_TOKEN"),
      Env[F].get("CLOUDFLARE_MODEL"),
    ).mapN { (accountId, token, model) =>
      workersAI[F](accountId, ApiKey(token), defaultModel = model.fold(ModelId.clef)(ModelId(_)))
    }

  private def required[F[_]: Env: MonadThrow](name: String): F[String] =
    Env[F].get(name).flatMap(_.liftTo[F](JevError.MissingEnv(name)))

  private def bearer[F[_]](request: Request[F], key: ApiKey): Request[F] =
    request.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, key.value)))

}
