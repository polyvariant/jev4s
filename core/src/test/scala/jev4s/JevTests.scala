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

import cats.effect.IO
import cats.effect.Ref
import example.*
import io.circe.Json
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Uri
import org.http4s.Response
import org.http4s.Status
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.implicits.*
import org.typelevel.ci.*

import scala.concurrent.duration.*

class JevTests extends CatsEffectSuite {

  private val config = JevConfig(
    Provider.typeSafe[IO](ApiKey("test-key")),
    retry = JevConfig.RetryConfig.disabled,
  )

  private def fake(status: Status, body: Json, config: JevConfig[IO] = config)
    : IO[(Ref[IO, List[(String, Json)]], Jev[IO])] =
    Ref[IO].of(List.empty[(String, Json)]).map { seen =>
      val client = Client.fromHttpApp(HttpApp[IO] { req =>
        req
          .as[Json]
          .flatMap(j => seen.update(_ :+ (req.uri.renderString -> j)))
          .as(Response[IO](status).withEntity(body))
      })
      (seen, Jev.instance[IO](config, client))
    }

  private def json(s: String): Json = parse(s).fold(throw _, identity)

  test("typed triage round trip") {
    val response = json("""{
      "model": "jev-1.13.0",
      "answers": {
        "q0": {"type": "noul", "noul": 0.95},
        "q1": {"type": "choice", "choice": "billing", "probabilities": {"billing": 0.88, "technical": 0.12, "sales": 0.0}, "confidence": 0.81},
        "q2": {"type": "score", "score": 1.05, "legend": {"0": "Calm", "1": "Frustrated", "2": "Very angry"}, "probabilities": {"0": 0.0, "1": 0.95, "2": 0.05}, "confidence": 0.92},
        "q3": {"type": "noul", "noul": 0.1},
        "q4": {"type": "noul", "noul": 0.2},
        "q5": {"type": "noul", "noul": 0.3}
      },
      "usage": {"input_tokens": 318, "output_tokens": 34}
    }""")

    for {
      (seen, jev) <- fake(Status.Ok, response)
      result <- jev.evaluate("Help! My payouts have been failing for 3 days.", Triage.question)
      requests <- seen.get
    } yield {
      val triage = result.answers
      assertEquals(result.model, ModelId("jev-1.13.0"))
      assertEquals(triage.urgent.yes.value, 0.95)
      assertEquals(triage.department.choice, Department.Billing)
      assertEquals(triage.frustration.mostLikely, Frustration.Frustrated)
      assertEquals(triage.topics("data loss").yes.value, 0.3)

      val List((uri, body)) = requests: @unchecked
      assertEquals(uri, "https://api.typesafe.ai/v1/systemone")
      assertEquals(
        body.hcursor.downField("questions").downField("q1").focus,
        Some(json("""{
          "type": "choice",
          "instructions": "Which team should handle this ticket?",
          "criteria": {"billing": "Payments, invoicing, refunds", "technical": "Bugs, outages, integrations", "sales": "Pricing, upgrades, new accounts"}
        }""")),
      )
      assertEquals(
        body.hcursor.downField("questions").downField("q2").downField("criteria").focus,
        Some(json("""["Calm", "Frustrated", "Very angry"]""")),
      )
      assertEquals(body.hcursor.downField("model").focus, Some(Json.fromString("jev-latest")))
      assertEquals(
        body.hcursor.downField("questions").downField("q0").focus,
        Some(json("""{
          "type": "noul",
          "instructions": "Does `message` convey urgency?",
          "criteria": {"true": "Explicitly time-sensitive", "false": "No urgency expressed"}
        }""")),
      )
      assertEquals(
        body.hcursor.downField("questions").downField("q3").focus,
        Some(
          json("""{"type": "noul", "instructions": "Does the ticket ask about refund?"}""")
        ),
      )
    }
  }

  test("options derived from annotations") {
    val options = Options.derived[Plan]
    assertEquals(options.values.toVector, Vector(Plan.Free, Plan.Pro))
    assertEquals(options.values.toVector.map(options.label), Vector("Free", "paid"))
    assertEquals(
      options.values.toVector.map(options.description),
      Vector(Some(Json.fromString("No payment")), None),
    )
  }

  test("derivation rejects cases with fields") {
    assert(
      compileErrors("Options.derived[WithFields]").contains(
        "only parameterless cases can be options"
      ),
      compileErrors("Options.derived[WithFields]"),
    )
  }

  test("invalid questions fail before sending") {
    val tooFewLevels =
      Question.score(
        using Options.derived[Single]
      )("?")
    fake(Status.Ok, Json.obj())
      .flatMap((seen, jev) => jev.evaluate("x", tooFewLevels).attempt.product(seen.get))
      .map { (result, requests) =>
        assert(result.left.exists(_.isInstanceOf[JevError.InvalidQuestion]), result)
        assertEquals(requests, Nil)
      }
  }

  test("duplicate choice labels fail before sending") {
    val question =
      Question.choice(
        using Options.labels("a", "b", "a")
      )("?")
    fake(Status.Ok, Json.obj())
      .flatMap((seen, jev) => jev.evaluate("x", question).attempt.product(seen.get))
      .map { (result, requests) =>
        assertEquals(result, Left(JevError.InvalidQuestion("Duplicate Choice labels: a")))
        assertEquals(requests, Nil)
      }
  }

  private val workersAI =
    JevConfig(
      Provider.workersAI[IO]("acc123", ApiKey("cf-token")),
      retry = JevConfig.RetryConfig.disabled,
    )

  // As returned by the real API, envelope included.
  private val noulResponse = json("""{
    "result": {
      "model": "clef",
      "answers": {"q0": {"type": "noul", "noul": 0.7}},
      "usage": {"input_tokens": 10, "output_tokens": 0}
    },
    "success": true,
    "errors": [],
    "messages": []
  }""")

  test("Workers AI: model goes in the path and the body") {
    for {
      (seen, jev) <- fake(Status.Ok, noulResponse, workersAI)
      result <- jev.withModel(ModelId.clefFlash).evaluate("x", Question.noul("?"))
      requests <- seen.get
    } yield {
      assertEquals(result.answers.yes.value, 0.7)
      val List((uri, body)) = requests: @unchecked
      assertEquals(
        uri,
        "https://api.cloudflare.com/client/v4/accounts/acc123/ai/run/@cf/cloudflare/clef-flash",
      )
      assertEquals(body.hcursor.downField("model").focus, Some(Json.fromString("clef-flash")))
    }
  }

  test("Workers AI: listing models is unsupported") {
    fake(Status.Ok, Json.obj(), workersAI)
      .flatMap((seen, jev) => jev.models.attempt.product(seen.get))
      .map { (result, requests) =>
        assertEquals(result, Left(JevError.Unsupported("listing models")))
        assertEquals(requests, Nil)
      }
  }

  test("credentials are fetched for every attempt, retries included") {
    for {
      tokens <- Ref[IO].of(0)
      seen <- Ref[IO].of(List.empty[String])
      provider =
        new Provider[IO] {
          def defaultModel: ModelId = ModelId.latest
          def evaluateUri(model: ModelId): Uri = uri"https://example.com/eval"
          def modelsUri: Option[Uri] = None
          def authorize(request: Request[IO]): IO[Request[IO]] =
            tokens.updateAndGet(_ + 1).map(n => request.putHeaders(Header.Raw(ci"X-Token", s"t$n")))
        }
      client = Client.fromHttpApp(HttpApp[IO] { req =>
        val token = req.headers.get(ci"X-Token").fold("none")(_.head.value)
        seen.updateAndGet(_ :+ token).map { all =>
          if (all.sizeIs == 1)
            Response[IO](Status.ServiceUnavailable)
          else
            Response[IO](Status.Ok).withEntity(
              json(
                """{"model": "m", "answers": {"q0": {"type": "noul", "noul": 0.5}}, "usage": {"input_tokens": 1, "output_tokens": 0}}"""
              )
            )
        }
      })
      jev = Jev.instance[IO](
        JevConfig(provider, JevConfig.RetryConfig(maxRetries = 1, maxBackoff = 1.milli)),
        client,
      )
      _ <- jev.evaluate("x", Question.noul("?"))
      tokensSent <- seen.get
    } yield assertEquals(tokensSent, List("t1", "t2"))
  }

  test("422 is surfaced with its body") {
    fake(Status.UnprocessableContent, Json.obj("detail" -> Json.fromString("bad")))
      .flatMap((_, jev) => jev.evaluate("x", Question.noul("?")).attempt)
      .map(r =>
        assertEquals(r, Left(JevError.Unprocessable(Json.obj("detail" -> Json.fromString("bad")))))
      )
  }
}

enum Single {
  case Only
}

sealed trait Plan

object Plan {
  @description("No payment") case object Free extends Plan
  @label("paid") case object Pro extends Plan
}

enum WithFields {
  case Named(name: String)
  case Empty
}
