package server

import cats.effect.std.AtomicCell
import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.comcast.ip4s.IpLiteralSyntax
import linkgame.GameSession
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder

import java.util.UUID

object Server extends IOApp {
  private def httpApp(sessions: AtomicCell[IO, Map[UUID, GameSession]]): HttpApp[IO] ={
    Route(sessions).orNotFound
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      sessions <- AtomicCell.apply[IO].of(Map.empty[UUID, GameSession])
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8080")
        .withHttpApp(httpApp(sessions))
        .build
        .useForever
    } yield ExitCode.Success

}
