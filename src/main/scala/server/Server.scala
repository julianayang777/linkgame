package server

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.IpLiteralSyntax
import fs2.Stream
import fs2.concurrent.Topic
import linkgame.GameLevel.Easy
import linkgame.{GameSession, Player}
import linkgame.GameStatus.InProgress
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

import java.util.UUID
import scala.concurrent.duration.DurationInt

object Server extends IOApp {
  private def httpApp(
    wsb: WebSocketBuilder2[IO],
    sessions: AtomicCell[IO, Map[UUID, GameSession]],
    q: Queue[IO, WebSocketFrame],
    t: Topic[IO, WebSocketFrame],
  ): HttpApp[IO] = {
    Route(wsb, sessions, q, t).orNotFound
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      board    <- linkgame.Board.initBoard(Easy) // For TESTING
      q        <- Queue.unbounded[IO, WebSocketFrame]
      t        <- Topic[IO, WebSocketFrame]
      sessions <- AtomicCell
        .apply[IO]
        .of(
          // Map.empty[UUID, GameSession]
          // For TESTING
          Map(
            UUID.fromString(
            "15f6cead-81cd-4eeb-8aa3-6605e30e966c") -> GameSession(board, InProgress, Player("player1"))
          )
        )

      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8080")
        // .withHttpApp(httpApp(sessions))
        .withHttpWebSocketApp(wsb => httpApp(wsb, sessions, q, t))
        .build
        .useForever

      _ <- Stream(
        Stream.fromQueueUnterminated(q).through(t.publish),
        // do a Ping to out topic so that the socket wont close after 60 sec
//        Stream
//          .awakeEvery[IO](30.seconds)
//          .map(_ => WebSocketFrame.Ping())
//          .through(t.publish),
        Stream.eval(server)
      ).parJoinUnbounded.compile.drain
    } yield ExitCode.Success
  }

}
