package server.leaderboard

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.implicits._
import linkgame.game.GameLevel
import server.leaderboard.LeaderboardService.Score
import server.player.PlayerService.PlayerId

trait LeaderboardService {
  def addScore(level: GameLevel, id: PlayerId, score: Score): IO[Unit]
  def getTopPlayers(level: GameLevel, topN: Int): IO[List[(PlayerId, Score)]]
  def getBestScore(id: PlayerId): IO[Map[GameLevel, Score]]
}

object LeaderboardService {
  type Score = Long // Duration

  // Assume that the received player exists in the system
  def inMemory: IO[LeaderboardService] =
    for {
      leaderboardRef <- Ref.of[IO, Map[GameLevel, Map[PlayerId, Score]]](Map.empty)
    } yield new LeaderboardService {
      override def addScore(level: GameLevel, id: PlayerId, newScore: Score): IO[Unit] =
        leaderboardRef.modify { leaderboard =>
          val playerBoard: Map[PlayerId, Score] = leaderboard.getOrElse(level, Map.empty)
          playerBoard.get(id) match {
            case Some(score) if newScore >= score => (leaderboard, ().asRight)
            case _                                =>
              val newPlayerBoard = playerBoard.updated(id, newScore)
              (leaderboard.updated(level, newPlayerBoard), ().asRight)
          }
        }

      override def getTopPlayers(level: GameLevel, topN: Int): IO[List[(PlayerId, Score)]] =
        leaderboardRef.get.map { leaderboard =>
          leaderboard.get(level) match {
            case Some(playersBoard) =>
              playersBoard.toList.sortBy(_._2).take(topN)
            case None               => List.empty
          }
        }

      override def getBestScore(id: PlayerId): IO[Map[GameLevel, Score]] = {
        leaderboardRef.get.map { leaderboard =>
          leaderboard.collect {
            case (level, playersBoard) if playersBoard.contains(id) => level -> playersBoard(id)
          }
        }
      }
    }
}
