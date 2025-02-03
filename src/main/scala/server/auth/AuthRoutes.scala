package server.auth

import cats.effect.{IO, Ref}
import linkgame.player.Player.PlayerId
import org.http4s.{AuthedRoutes, HttpRoutes}
import cats.syntax.all._
import linkgame.player.Player
import org.http4s.dsl.io._
import org.http4s.server.AuthMiddleware

import java.util.UUID

object AuthRoutes {
  def apply(authService: AuthService, authMiddleware: AuthMiddleware[IO, PlayerId]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] { case POST -> Root / "auth" / "signIn" / username =>
     for {
       signInResult <- authService.signIn(username)
       response <- signInResult match {
         case Left(e) => BadRequest(e.toString)
         case Right(authToken) => Ok(authToken)
       }
     } yield response
    } <+> authMiddleware {
      AuthedRoutes.of[PlayerId, IO] {
        case GET -> Root / "auth" / "login" as playerId =>
          Ok(s"Hello, $playerId")
      }
    }
  }

}
