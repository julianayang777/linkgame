package server.leaderboard

import scala.util.control.NoStackTrace

sealed trait LeaderboardError extends Throwable

case object PlayerNotFound extends LeaderboardError with NoStackTrace
