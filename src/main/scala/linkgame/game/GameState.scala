package linkgame.game

import cats.effect.IO
import cats.implicits._
import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import linkgame.game.Board.{Board, deleteTileFromBoard, initBoard, isCoordinateOnBoard, isEmpty, isSolvable, isValidPath, printBoard, refreshBoard}
import linkgame.game.GameState.GameError
import linkgame.player.Player

import java.time.Instant

sealed trait GameState {
  def join(player: Player): Either[GameError, IO[GameState]] =
    this match {
      // TODO multiplayers
      case GameState.AwaitingPlayers(players, gameLevel) =>
        (for {
          start <- IO.realTimeInstant
          board <- initBoard(gameLevel)
          _     <- printBoard(board)
        } yield GameState.InProgress(player, board, start)).asRight
      case inProgress: GameState.InProgress              =>
        if (player == inProgress.player) {
          (for {
            _ <- printBoard(inProgress.board)
          } yield inProgress).asRight
        } else { GameError.GameAlreadyStarted.asLeft[IO[GameState]] }
      case _: GameState.Win                              => GameError.GameAlreadyEnded.asLeft
    }

  def attemptMatch(p1: Coordinate, p2: Coordinate): Either[GameError, IO[GameState]] = {
    // TODO: for multiplayers
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
}

object GameState {

  sealed trait GameError

  final case class AwaitingPlayers(players: Set[Player], gameLevel: GameLevel) extends GameState

  final case class InProgress(player: Player, board: Board, startInstant: Instant) extends GameState

  final case class Win(winner: Player, startInstant: Instant, endInstant: Instant) extends GameState

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
