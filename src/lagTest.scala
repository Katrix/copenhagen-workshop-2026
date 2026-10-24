import calico.html.io.{*, given}
import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.*
import org.http4s.client.websocket.*
import org.http4s.dom.*
import org.http4s.syntax.all.*
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

import cats.data.Chain
import fs2.concurrent.SignallingRef

def lagTest = SignallingRef.of[IO, Chain[FiniteDuration]](Chain.empty).toResource.flatMap { lagSignal =>
  WebSocketClient[IO].connectHighLevel(WSRequest(uri"wss://echo.websocket.org")).flatMap { ws =>
    val sendStream = Stream.fixedRateStartImmediately[IO](1.second).evalMap { _ =>
      IO.realTime.flatMap(rt => ws.send(WSFrame.Text(rt.toMillis.toString)))
    }

    ws.receiveStream
      .zip(Stream.repeatEval(IO.realTime))
      .concurrently(sendStream)
      .mapFilter { case (WSFrame.Text(rawTs, _), ts) =>
        rawTs.toLongOption.map(ts - _.millis)
      }
      .evalTap(delay => IO.println(delay) >> lagSignal.update(_ :+ delay))
      .hold1Resource
      .flatMap { sig =>
        div(
          h3("Lag Test"),
          p(
            "This widget sends a timestamp to the echo websocket server every second and displays the last received timestamp."
          ),
          p("The value below should update every second with the current timestamp sent to the server."),
          p(sig.map(_.toCoarsest.toString)),
          table(lagSignal.map { lags =>
            tbody(
              tr(th("Lag (ms)")) ::
                lags.toList.reverse.map { lag =>
                  tr(td(lag.toCoarsest.toString))
                }
            )
          })
        )
      }
  }
}
