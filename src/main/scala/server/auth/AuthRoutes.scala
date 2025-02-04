package server.auth

import cats.effect.IO
import cats.syntax.all._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.dsl.io._
import org.http4s.server.AuthMiddleware
import org.http4s.{AuthedRoutes, HttpRoutes}
import server.Request.SignUp
import server.player.PlayerService.PlayerId

object AuthRoutes {
  def apply(authService: AuthService, authMiddleware: AuthMiddleware[IO, PlayerId]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "auth" / "signup" =>
        for {
          request      <- req.as[SignUp]
          signupResult <- authService.signup(request.username)
          response     <- signupResult match {
            case Left(e)        => BadRequest(e.toString)
            case Right(message) => Ok(message)
          }
        } yield response

      case POST -> Root / "auth" / "login" / username =>
        for {
          // loginResult <- authService.login(UUID.fromString(username))
          loginResult <- authService.login(username)
          response    <- loginResult match {
            case Left(e)          => BadRequest(e.toString)
            case Right(authToken) => Ok(authToken)
          }
        } yield response
    } <+> authMiddleware {
      AuthedRoutes.of[PlayerId, IO] { case GET -> Root / "auth" / "info" as playerId =>
        Ok(s"Hello, $playerId")
      }
    }
  }

}
