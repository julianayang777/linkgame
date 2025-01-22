package server

import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.comcast.ip4s.IpLiteralSyntax
import linkgame.GameSession
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling

import java.util.UUID

object Server extends IOApp {
  private def httpApp(sessions: Ref[IO, Map[UUID, GameSession]]): HttpApp[IO] ={
    Route(sessions).orNotFound
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      sessions <- Ref.of[IO, Map[UUID, GameSession]](Map.empty[UUID, GameSession])
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8080")
        .withHttpApp(httpApp(sessions))
        .build
        .useForever
    } yield ExitCode.Success

}
