package linkgame.game

import cats.effect.IO
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.circe.generic.extras.semiauto.{deriveConfiguredCodec, deriveEnumerationCodec}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import linkgame.game.Board.{
  Board,
  deleteTileFromBoard,
  isCoordinateOnBoard,
  isEmpty,
  isSolvable,
  isValidPath,
  refreshBoard,
}
import linkgame.game.GameState.GameError
import linkgame.player.Player
import cats.implicits._

import java.time.Instant

sealed trait GameState {
  def join(player: Player): Either[GameError, GameState] =
    this match {
      case GameState.AwaitingPlayers(players)                => ???
      case GameState.InProgress(player, board, startInstant) => ???
      case GameState.Win(winner, startInstant, endInstant)   => ???
    }

  def makeMove(p1: Coordinate, p2: Coordinate): Either[GameError, IO[GameState]] =
    this match {
      case inProgress @ GameState.InProgress(player, board, startInstant) =>
        for {
          _ <- Either.cond(isCoordinateOnBoard(board, p1), (), GameError.CoordinatesOutOfBounds)
          _ <- Either.cond(isCoordinateOnBoard(board, p2), (), GameError.CoordinatesOutOfBounds)
          _ <- Either.cond(isValidPath(board, (p1.row, p1.column), (p2.row, p2.column)), (), GameError.InvalidMatch)

        } yield {
          val updatedBoard1 = deleteTileFromBoard(board, p1)
          val updatedBoard2 = deleteTileFromBoard(updatedBoard1, p2)
          if (isEmpty(updatedBoard2)) {
            for {
              end <- IO.realTimeInstant
            } yield (GameState.Win(player, startInstant, end))
          } else {
            for {
              newBoard <-
                if (isSolvable(updatedBoard2)) { IO.pure { updatedBoard2 } }
                else { refreshBoard(updatedBoard2) }
            } yield (inProgress.copy(board = newBoard))
          }
        }
      case _: GameState.AwaitingPlayers                                   => GameError.GameNotStarted.asLeft
      case _: GameState.Win                                               => GameError.GameAlreadyEnded.asLeft
    }
}

object GameState {
  final case class AwaitingPlayers(players: Set[Player])                           extends GameState
  final case class InProgress(player: Player, board: Board, startInstant: Instant) extends GameState
  final case class Win(winner: Player, startInstant: Instant, endInstant: Instant) extends GameState

  sealed trait GameError

  object GameError {
    case object GameAlreadyStarted     extends GameError
    case object GameAlreadyEnded       extends GameError
    case object GameNotStarted         extends GameError
    case object CoordinatesOutOfBounds extends GameError
    case object InvalidMatch           extends GameError
  }

  implicit val codec: Codec[GameState] = {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("type")
    deriveConfiguredCodec
  }

}
