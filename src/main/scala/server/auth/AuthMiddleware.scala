package server.auth

import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import org.http4s.Credentials.Token
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Request}
import pdi.jwt.{Jwt, JwtAlgorithm, JwtOptions}
import server.Config.SecretConfigValue
import server.player.PlayerService.PlayerId

object AuthMiddleware {
  def apply(jwtSecret: SecretConfigValue[String]): org.http4s.server.AuthMiddleware[IO, PlayerId] = {
    def getBearerToken(request: Request[IO]): Option[String] =
      request.headers
        .get[Authorization]
        .collect { case Authorization(Token(AuthScheme.Bearer, token)) => token }
        .orElse {
          // Fallback for WS routes
          request.params.get("authToken")
        }

    val authUser = Kleisli { request: Request[IO] =>
      for {
        token    <- OptionT.fromOption[IO](getBearerToken(request))
        jwtClaim <- OptionT(
          IO.delay {
            Jwt
              .decode(
                token = token,
                key = jwtSecret.value,
                algorithms = Seq(JwtAlgorithm.HS512),
                options = JwtOptions.DEFAULT,
              )
              .toOption
          }
        )
        username <- OptionT.fromOption[IO](jwtClaim.subject)
      } yield username
    }

    org.http4s.server.AuthMiddleware.withFallThrough(authUser)
  }
}
