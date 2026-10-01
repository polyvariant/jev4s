# jev4s

A typed, purely functional Scala client for [TypeSafe](https://typesafe.ai)'s System One models (Jev), built on cats-effect and http4s.

Available for Scala 3 on the JVM, Scala.js and Scala Native.

> [!WARNING]
> jev4s is at an early stage. Only snapshots are published so far, and the API may change without notice.

## Installation

Snapshots are published to the Sonatype Central snapshots repository on every push to `main`:

```scala
resolvers += "central-snapshots" at "https://central.sonatype.com/repository/maven-snapshots/"

libraryDependencies += "org.polyvariant" %%% "jev4s" % "<version>"
```

You'll need an HTTP client too, for example `"org.http4s" %%% "http4s-ember-client"`.

## Usage

Describe what you want to know about some state as a `Question`, then evaluate it. There are three kinds of question:

- `Question.noul`: a yes/no question, answered with the probability of "yes" (`Noul`).
- `Question.choice[A]`: picks one of `A`'s options (`Choice[A]`).
- `Question.score[A]`: rates the state on `A`'s levels, lowest first (`Score[A]`).

Options for choices and scores can be derived from an enum, with annotations for the label sent to the model and a description of each case:

```scala
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
```

`Question` is an `Applicative`, so questions compose with `mapN`, `tupled`, `traverse` and friends into a single question with a typed answer. All of them are sent in one request and evaluated independently:

```scala
import cats.syntax.all.*

final case class Triage(
  urgent: Noul,
  department: Choice[Department],
  frustration: Score[Frustration],
  topics: Map[String, Noul],
)

val triage: Question[Triage] =
  (
    Question.noul(
      "Does `message` convey urgency?",
      yes = "Explicitly time-sensitive",
      no = "No urgency expressed",
    ),
    Question.choice[Department]("Which team should handle this ticket?"),
    Question.score[Frustration]("How frustrated is the customer?"),
    List("refund", "account access", "data loss")
      .traverse(t => Question.noul(s"Does the ticket ask about $t?").tupleLeft(t))
      .map(_.toMap),
  ).mapN(Triage.apply)
```

The state can be anything with a circe `Encoder`: a plain `String`, or a case class / map / list for structured context.

```scala
import cats.effect.*
import io.circe.Encoder
import org.http4s.ember.client.EmberClientBuilder

final case class Ticket(subject: String, message: String) derives Encoder.AsObject

object Main extends IOApp.Simple {

  val run: IO[Unit] =
    EmberClientBuilder.default[IO].build.use { client =>
      for {
        config <- JevConfig.fromEnv[IO]
        jev = Jev.instance[IO](config, client)
        result <- jev.evaluate(
          Ticket("Payouts", "Help! My payouts have been failing for 3 days."),
          triage,
        )
        _ <- IO.println(s"department: ${result.answers.department.choice}")
        _ <- IO.println(s"urgent: ${result.answers.urgent.yes.value}")
      } yield ()
    }

}
```

Complete examples live in [`core/src/test/scala/example`](core/src/test/scala/example): [`Triage.scala`](core/src/test/scala/example/Triage.scala) is the one above, and [`VibeCheck.scala`](core/src/test/scala/example/VibeCheck.scala) is an interactive one. Type messages, and each one gets judged in the context of the conversation so far:

```
> Hey! Did you manage to fix the deploy pipeline?
mood: 🙂 Cheerful (2.83 of 4)
intent: Question 99%
sarcastic: 9%
on topic: 95%

> Oh great, it's broken AGAIN. Wonderful. Just what I needed today.
mood: 😒 Grumpy (0.99 of 4)
intent: Complaint 100%
sarcastic: 97%
on topic: 92%
```

## Configuration

`JevConfig.fromEnv` reads the same environment variables as the official SDKs:

| Variable | Required | Default |
| --- | --- | --- |
| `TYPESAFE_API_KEY` | yes | |
| `TYPESAFE_BASE_URL` | no | `https://api.typesafe.ai` |
| `TYPESAFE_DEFAULT_MODEL` | no | `jev-latest` |

You can also construct a `JevConfig` directly. Requests that fail with 408, 429 or 5xx are retried with exponential backoff (2 retries by default, see `JevConfig.RetryConfig`). Timeouts and connection pooling are up to the `Client` you provide.

To use a different model for some requests, use `jev.withModel(ModelId("jev-1.13.0"))`. `jev.models` lists the available models.

### Cloudflare Workers AI (Clef)

Cloudflare's [Clef models](https://developers.cloudflare.com/workers-ai/models/clef/) speak the same System One format, so the same questions work against them:

```scala
val config = JevConfig.workersAI(accountId, ApiKey(apiToken)) // or JevConfig.workersAIFromEnv[IO]
val jev = Jev.instance[IO](config, client).withModel(ModelId.clefFlash)
```

`JevConfig.workersAIFromEnv` reads `CLOUDFLARE_ACCOUNT_ID`, `CLOUDFLARE_AUTH_TOKEN` and optionally `CLOUDFLARE_MODEL` (`clef` by default). The token needs the "Workers AI - Read" and "Workers AI - Edit" permissions. `jev.models` isn't supported on Workers AI. `example.ClefTriage` runs the triage example against Clef.

## License

Licensed under the [Apache License, Version 2.0](LICENSE).
