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

package example

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.all.*
import io.circe.Encoder
import jev4s.*
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.*
import scala.math.BigDecimal.RoundingMode

/** An interactive vibe check: type messages, and each one is judged in the context of the
  * conversation so far. An empty line or EOF quits.
  */
object VibeCheck extends IOApp.Simple {

  enum Mood(val emoji: String) derives Options {
    @description("Hostile, insulting") case Hostile extends Mood("🤬")
    @description("Grumpy, annoyed") case Grumpy extends Mood("😒")
    @description("Neutral, matter-of-fact") case Neutral extends Mood("😐")
    @description("Friendly, cheerful") case Cheerful extends Mood("🙂")
    @description("Ecstatic, overjoyed") case Ecstatic extends Mood("🤩")
  }

  enum Intent derives Options {
    @label("question") @description("Asks for information") case Question
    @label("request") @description("Asks someone to do something") case Request
    @label("complaint") @description("Expresses dissatisfaction") case Complaint
    @label("compliment") @description("Praises or thanks someone") case Compliment
    @label("small talk") @description("Chit-chat with no particular goal") case SmallTalk
  }

  final case class Conversation(earlier: List[String], latest: String) derives Encoder.AsObject

  final case class Vibe(mood: Score[Mood], intent: Choice[Intent], sarcastic: Noul, onTopic: Noul)

  val vibe: Question[Vibe] =
    (
      Question.score[Mood]("What is the mood of the `latest` message?"),
      Question.choice[Intent]("What is the `latest` message trying to achieve?"),
      Question.noul(
        "Is the `latest` message sarcastic?",
        yes = "Says the opposite of what it means",
        no = "Means what it says",
      ),
      Question.noul(
        "Does the `latest` message stay on the topic of the `earlier` messages?",
        yes = "Continues the conversation, or there are no earlier messages",
        no = "Changes the subject",
      ),
    ).mapN(Vibe.apply)

  private def percent(p: Probability): String = s"${(p.value * 100).round}%"

  def render(v: Vibe): List[String] = {
    val intents = v
      .intent
      .probabilities
      .toList
      .sortBy(-_._2.value)
      .takeWhile(_._2.value >= 0.05)
      .map((intent, p) => s"$intent ${percent(p)}")

    List(
      s"mood: ${v.mood.mostLikely.emoji} ${v.mood.mostLikely} " +
        s"(${BigDecimal(v.mood.score).setScale(2, RoundingMode.HALF_UP)} of ${Mood.values.length - 1})",
      s"intent: ${intents.mkString(", ")}",
      s"sarcastic: ${percent(v.sarcastic.yes)}",
      s"on topic: ${percent(v.onTopic.yes)}",
    )
  }

  private def loop(jev: Jev[IO], earlier: List[String]): IO[Unit] =
    for {
      _ <- IO.print("> ")
      // `option` turns EOF into None.
      line <- Console[IO].readLine.option.map(_.map(_.trim).filter(_.nonEmpty))
      _ <- line.traverse_ { message =>
        for {
          result <- jev.evaluate(Conversation(earlier, message), vibe).attempt
          _ <- result.fold(
            e => IO.println(s"error: ${e.getMessage}"),
            r => render(r.answers).traverse_(IO.println),
          )
          _ <- IO.println("")
          // Keep the context small: only the last few messages.
          _ <- loop(jev, (earlier :+ message).takeRight(5))
        } yield ()
      }
    } yield ()

  val run: IO[Unit] =
    EmberClientBuilder.default[IO].withTimeout(30.seconds).build.use { client =>
      for {
        provider <- Provider.typeSafeFromEnv[IO]
        _ <- IO.println("Say something (empty line to quit).")
        _ <- loop(Jev.instance[IO](JevConfig(provider), client), Nil)
      } yield ()
    }

}
