package server

import cats.effect.implicits._
import cats.effect.std.AtomicCell
import cats.effect.{IO, Ref, Resource, ResourceApp}
import cats.syntax.all._
import linkgame.player.Player.PlayerId
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.CatsEffectConfigSource
import server.auth.{AuthMiddleware, AuthRoutes, AuthService}

import java.util.UUID

object Server extends ResourceApp.Forever {

  private def httpApp(
    wsb: WebSocketBuilder2[IO],
    rooms: Ref[IO, Map[UUID, AtomicCell[IO, GameRoom]]],
    authService: AuthService,
    authMiddleware: org.http4s.server.AuthMiddleware[IO, PlayerId]
  ): HttpApp[IO] = {
    (Route(wsb, rooms) <+> AuthRoutes(authService,authMiddleware)).orNotFound
  }

  override def run(args: List[String]): Resource[IO, Unit] = {
    for {
      config <- ConfigSource.default.loadF[IO, Config].toResource

      gameRoomsRef <- Ref.of[IO, Map[UUID, AtomicCell[IO, GameRoom]]](Map.empty).toResource

      authService <- AuthService.inMemory(config.jwtSecret, config.jwtExpirationTime).toResource
      authMiddleware = AuthMiddleware(config.jwtSecret)

      _         <- EmberServerBuilder
        .default[IO]
        .withHost(config.serverHost)
        .withPort(config.serverPort)
        .withHttpWebSocketApp(wsb => httpApp(wsb, gameRoomsRef, authService, authMiddleware))
        .build
    } yield ()
  }

}
