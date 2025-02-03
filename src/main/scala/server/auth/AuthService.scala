package server.auth

import cats.effect.{IO, Ref}
import cats.implicits._
import linkgame.player.Player.PlayerId
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}
import server.Config.SecretConfigValue
import server.auth.AuthService.AuthToken

import scala.concurrent.duration.FiniteDuration

trait AuthService {
  def signIn(username: PlayerId): IO[Either[AuthError, AuthToken]]
}

object AuthService {
  type AuthToken = String

  def inMemory(jwtSecret: SecretConfigValue[String],
               jwtExpirationTime: FiniteDuration): IO[AuthService] =
    for {
      usersRef <- Ref.of[IO, Set[PlayerId]](Set.empty)
    } yield new AuthService {
      override def signIn(username: PlayerId): IO[Either[AuthError, AuthToken]] =
        for {
          userAlreadyExists <- usersRef.modify { users =>
            val alreadyExists = users.contains(username)
            (users + username, alreadyExists)
          }

          now <- IO.realTimeInstant

          result <-
            if (userAlreadyExists) UserAlreadyExists.asLeft.pure[IO]
            else
              IO.delay {
                val issuedAtSeconds = now.getEpochSecond

                val claim = JwtClaim(
                  subject = username.some,
                  issuedAt = issuedAtSeconds.some,
                  expiration = (issuedAtSeconds + jwtExpirationTime.toSeconds).some
                )

                Jwt
                  .encode(
                    claim = claim,
                    algorithm = JwtAlgorithm.HS512,
                    key = jwtSecret.value
                  )
                  .asRight
              }
        } yield result
    }
}
