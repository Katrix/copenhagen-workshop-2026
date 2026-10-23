import calico.html.io.{*, given}
import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.*

def fibonacci = Queue.unbounded[IO, Unit].toResource.flatMap { clicks =>
  (Stream.unit ++ Stream.fromQueueUnterminated(clicks))
    .zipRight(
      Stream.iterate((BigInt(0), BigInt(1))) { case (a, b) => (b, a + b) }.map { case (a, b) => a + b }.zipWithIndex
    )
    .hold1Resource
    .flatMap { signal =>
      div(
        button(
          "Press me to get the next Fibonacci number",
          onClick.apply(clicks.offer(()))
        ),
        p("Current value: ", signal.map { (fib, idx) => s"F(${idx + 2}) = $fib" })
      )
    }
}
