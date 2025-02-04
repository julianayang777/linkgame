package server.auth

import cats.effect.{IO, Ref}
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
  def logout(id: PlayerId): IO[Either[AuthError, Unit]]
  def isLogged(id: PlayerId): IO[Boolean]
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

    for {
      sessionsRef <- Ref.of[IO, Set[PlayerId]](Set.empty)
    } yield new AuthService {
      override def signup(name: String): IO[Either[AuthError, String]] =
        for {
          // TODO: PlayerId == name, distinguish id with name
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
                userAlreadyLoggedIn <- sessionsRef.modify { sessions =>
                  val alreadyExists = sessions.contains(id)
                  (sessions + id, alreadyExists)
                }
                result              <-
                  if (userAlreadyLoggedIn) UserAlreadyLoggedIn.asLeft.pure[IO]
                  else
                    IO.delay {
                      val issuedAtSeconds = now.getEpochSecond
                      generateJwtToken(id, issuedAtSeconds, issuedAtSeconds + jwtExpirationTime.toSeconds).asRight
                    }
              } yield result
            } else UserNotFound.asLeft.pure[IO]
        } yield result

      override def logout(id: PlayerId): IO[Either[AuthError, Unit]] = {
        for {
          removed <- sessionsRef.modify { sessions =>
            (sessions - id, sessions.contains(id))
          }
          result   = if (removed) ().asRight else UserNotLoggedIn.asLeft
        } yield result
      }

      override def isLogged(id: PlayerId): IO[Boolean] = sessionsRef.get.map(_.contains(id))
    }
  }
}
