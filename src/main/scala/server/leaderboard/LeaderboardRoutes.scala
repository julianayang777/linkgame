package server.leaderboard

import cats.effect.IO
import cats.implicits._
import linkgame.game.GameLevel
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io._
import org.http4s.server.AuthMiddleware
import org.http4s.{AuthedRoutes, HttpRoutes}
import server.player.PlayerService.PlayerId

object LeaderboardRoutes {
  def apply(leaderboardService: LeaderboardService, authMiddleware: AuthMiddleware[IO, PlayerId]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] { case GET -> Root / "leaderboard" / GameLevel(level) / "top" / topN =>
      topN.toIntOption match {
        case Some(n) =>
          for {
            leaderboardResult <- leaderboardService.getTopPlayers(level, n)
            response          <- Ok(leaderboardResult)
          } yield response
        case None    => BadRequest("Invalid topN value, must be an integer")
      }
    } <+> authMiddleware(
      AuthedRoutes.of[PlayerId, IO] { case GET -> Root / "leaderboard" / "player" / "best-scores" as playerId =>
        for {
          scores <- leaderboardService.getBestScore(playerId)
          response <- Ok(scores)
        } yield response

      }
    )
  }
}
