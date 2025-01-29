package server

import cats.effect.implicits.effectResourceOps
import cats.effect.std.AtomicCell
import cats.effect.{IO, Ref, Resource, ResourceApp}
import com.comcast.ip4s.IpLiteralSyntax
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2

import java.util.UUID

object Server extends ResourceApp.Forever {
  private def httpApp(
    wsb: WebSocketBuilder2[IO],
    rooms: Ref[IO, Map[UUID, AtomicCell[IO, GameRoom]]],
  ): HttpApp[IO] = {
    Route(wsb, rooms).orNotFound
  }

  override def run(args: List[String]): Resource[IO, Unit] = {
    for {
      gameRooms <- Ref.of[IO, Map[UUID, AtomicCell[IO, GameRoom]]](Map.empty).toResource
      _         <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"8080")
        .withHttpWebSocketApp(wsb => httpApp(wsb, gameRooms))
        .build
    } yield ()
  }

}
