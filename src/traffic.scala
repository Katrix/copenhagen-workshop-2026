import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.*
import fs2.dom.*

enum Lamp:
  case Red, Yellow, Green

enum Light(val lights: Set[Lamp]):
  case Red       extends Light(Set(Lamp.Red))
  case RedYellow extends Light(Set(Lamp.Red, Lamp.Yellow))
  case Green     extends Light(Set(Lamp.Green))
  case Yellow    extends Light(Set(Lamp.Yellow))

  def next: Light = this match
    case Red       => RedYellow
    case RedYellow => Green
    case Green     => Yellow
    case Yellow    => Red

def traffic: Resource[IO, HtmlElement[IO]] =

  SignallingRef[IO].of(Light.Red).toResource.flatMap { state =>
    import Lamp.*
    def lamp(lamp: Lamp, color: String) =
      div(styleAttr <-- state.map { current =>
        val fill = if current.lights(lamp) then color else "#333"
        s"width: 4em; height: 4em; border-radius: 50%; margin: 0.5em; background: $fill"
      })

    div(
      div(
        styleAttr := "display: inline-block; padding: 0.5em; background: #111; border-radius: 1em",
        lamp(Red, "red"),
        lamp(Yellow, "gold"),
        lamp(Green, "limegreen")
      ),
      p(state.map(_.toString)),
      button("Next", onClick(state.update(_.next)))
    )
  }
