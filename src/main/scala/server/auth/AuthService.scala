package server.auth

import cats.effect.IO
import cats.implicits._
import pdi.jwt._
import server.Config.SecretConfigValue
import server.auth.AuthService.AuthToken
import server.player.PlayerService.PlayerId
import server.player.{PlayerAlreadyExists, PlayerNotFound, PlayerService}

import scala.concurrent.duration.FiniteDuration

trait AuthService {
  def signup(name: String): IO[Either[AuthError, String]]
  def login(id: PlayerId): IO[Either[AuthError, AuthToken]]
}

object AuthService {
  type AuthToken = String

  def inMemory(
    jwtSecret: SecretConfigValue[String],
    jwtExpirationTime: FiniteDuration,
    playerService: PlayerService,
  ): IO[AuthService] = {
    def generateJwtToken(id: PlayerId, issuedAtSeconds: Long, expirationTime: Long): AuthToken = {
      val claim = JwtClaim(
        subject = id.toString.some,
        issuedAt = issuedAtSeconds.some,
        expiration = expirationTime.some,
      )

      Jwt
        .encode(
          claim = claim,
          algorithm = JwtAlgorithm.HS512,
          key = jwtSecret.value,
        )
    }

    IO(new AuthService {
      override def signup(name: String): IO[Either[AuthError, String]] =
        for {
          maybePlayer <- playerService.addPlayer(name)
          result      <- maybePlayer.fold(
            {
              case PlayerAlreadyExists => UserAlreadyExists.asLeft.pure[IO]
              case PlayerNotFound      => UnexpectedError.asLeft.pure[IO]
            },
            { case (playerId, _) =>
              IO.pure(s"Player with ID ${playerId} created successfully".asRight)
            },
          )
        } yield result

      override def login(id: PlayerId): IO[Either[AuthError, AuthToken]] =
        for {
          now            <- IO.realTimeInstant
          containsPlayer <- playerService.containsPlayer(id)
          result         <-
            if (containsPlayer) {
              for {
                result <-
                  IO.delay {
                    val issuedAtSeconds = now.getEpochSecond
                    generateJwtToken(id, issuedAtSeconds, issuedAtSeconds + jwtExpirationTime.toSeconds).asRight
                  }
              } yield result
            } else UserNotFound.asLeft.pure[IO]
        } yield result
    })
  }
}
