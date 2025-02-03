package server.auth

import cats.effect.IO
import cats.syntax.all._
import dev.profunktor.auth.JwtAuthMiddleware
import dev.profunktor.auth.jwt.{JwtAuth, JwtToken}
import linkgame.player.Player.PlayerId
import pdi.jwt.{JwtAlgorithm, JwtClaim}
import server.Config.SecretConfigValue

object AuthMiddleware {
  def apply(jwtSecret: SecretConfigValue[String]): org.http4s.server.AuthMiddleware[IO, PlayerId] = {
    val jwtAuth = JwtAuth.hmac(jwtSecret.value, JwtAlgorithm.HS512)
    val authenticate: JwtToken => JwtClaim => IO[Option[PlayerId]] = {
      (_: JwtToken) =>
        (claim: JwtClaim) =>
          claim.subject.pure[IO]
    }

    JwtAuthMiddleware[IO, PlayerId](jwtAuth, authenticate)
  }

}