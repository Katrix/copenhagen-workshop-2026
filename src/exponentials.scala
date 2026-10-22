import calico.*
import calico.html.io.{*, given}
import cats.syntax.all.*
import cats.effect.*
import fs2.concurrent.SignallingRef
import fs2.*
import fs2.dom.*

def exponentials = SignallingRef.of[IO, (base: BigInt, exp: BigInt)](BigInt(2) -> BigInt(2)).toResource.flatMap { ref =>
  div(
    h3("Let's do some fun math here!"),
    label("Base: "),
    input.withSelf { self =>
      (
        `type` := "number",
        value <-- ref.map(_.base.toString),
        onChange(
          self.value.get.map(BigInt(_)).flatMap { newValue =>
            ref.update { case (_, exp) => (newValue, exp) }
          }
        )
      )
    },
    label("Exponent: "),
    input.withSelf { self =>
      (
        `type` := "number",
        value <-- ref.map(_.exp.toString),
        onChange(
          self.value.get.map(BigInt(_)).flatMap { newValue =>
            ref.update { case (base, _) => (base, newValue) }
          }
        )
      )
    },
    span("Result: ", ref.map { case (base, exp) => s"$base ^ $exp = ${base.pow(exp.toInt)}" })
  )
}
