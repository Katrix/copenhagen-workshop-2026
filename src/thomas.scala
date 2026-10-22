import calico.*
import calico.html.io.{*, given}
import calico.unsafe.given
import calico.syntax.*
import cats.effect.*
import fs2.*
import fs2.concurrent.*
import fs2.dom.*

import scala.concurrent.duration.DurationInt

def fibonacciGame: Resource[IO, HtmlDivElement[IO]] = {
  SignallingRef[IO].of((0, 1)).toResource.flatMap { ns =>
    Stream
      .fixedRate[IO](1.second)
      .interruptWhen(ns.map((a, b) => a < 0))
      .as(1)
      .scanMonoid
      .map(_.toString)
      .holdOptionResource
      .flatMap { tick =>
        div(
          div(s"Your time: ", tick),
          button(
            "Next Fibonacci number",
            onClick --> {
              _.foreach(_ => ns.getAndUpdate((a, b) => if (a == 0) (1, 2) else (b, a + b)).void)
            }
          ),
          div("N #: ", ns.map((a, b) => b.toString))
        )
      }
  }
}
