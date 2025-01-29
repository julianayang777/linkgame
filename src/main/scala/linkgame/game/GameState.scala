package linkgame.game

import cats.effect.IO
import cats.implicits._
import io.circe.{Codec, Encoder, Decoder}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import linkgame.game.Board.{Board, deleteTileFromBoard, initBoard, isCoordinateOnBoard, isEmpty, isSolvable, isValidPath, printBoard, refreshBoard}
import linkgame.game.GameState.GameError
import linkgame.player.Player

import java.time.Instant

sealed trait GameState {
  val maxPlayersPerRoom: Int = 2

  def join(player: Player): Either[GameError, IO[GameState]] =
    this match {
      // TODO multiplayers
      case GameState.AwaitingPlayers(players, gameLevel) =>
        val newPlayers = players + player
        if (newPlayers.size < maxPlayersPerRoom) IO.pure(GameState.AwaitingPlayers(newPlayers, gameLevel)).asRight
        else {
          (for {
            start       <- IO.realTimeInstant
            board       <- initBoard(gameLevel)
            _           <- printBoard(board)
            playerBoards = newPlayers.map(player => player -> board).toMap
          } yield GameState.InProgress(playerBoards, start)).asRight
        }
      case inProgress: GameState.InProgress              =>
        if (inProgress.playerBoards.contains(player)) IO.pure(inProgress).asRight
        else GameError.GameAlreadyStarted.asLeft[IO[GameState]]
      case _: GameState.Win                              => GameError.GameAlreadyEnded.asLeft
    }

  def attemptMatch(player: Player, p1: Coordinate, p2: Coordinate): Either[GameError, IO[GameState]] = {
    this match {
      case inProgress @ GameState.InProgress(playerBoards, startInstant) =>
        val board = playerBoards.get(player).toRight(GameError.PlayerNotInRoom)
        for {
          board <- board
          _     <- Either.cond(isCoordinateOnBoard(board, p1), (), GameError.CoordinatesOutOfBounds)
          _     <- Either.cond(isCoordinateOnBoard(board, p2), (), GameError.CoordinatesOutOfBounds)
          _     <- Either.cond(isValidPath(board, (p1.row, p1.column), (p2.row, p2.column)), (), GameError.InvalidMatch)
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
              newPlayersBoards = playerBoards.updated(player, newBoard)
            } yield (inProgress.copy(playerBoards = newPlayersBoards))
          }
        }
      case _: GameState.AwaitingPlayers                                  => GameError.GameNotStarted.asLeft
      case _: GameState.Win                                              => GameError.GameAlreadyEnded.asLeft
    }
  }
}

object GameState {

  sealed trait GameError

  final case class AwaitingPlayers(players: Set[Player], gameLevel: GameLevel) extends GameState

  final case class InProgress(playerBoards: Map[Player, Board], startInstant: Instant) extends GameState

  final case class Win(winner: Player, startInstant: Instant, endInstant: Instant) extends GameState

  import io.circe.syntax._ // For .asJson

  implicit val playerBoardMapCodec: Encoder[Map[Player, Board]] = Encoder.encodeMap[Player, Board]
  implicit val playerBoardMapDecoder: Decoder[Map[Player, Board]] = Decoder.decodeMap[Player, Board]


  object GameError {
    case object GameAlreadyStarted     extends GameError
    case object GameAlreadyEnded       extends GameError
    case object GameNotStarted         extends GameError
    case object CoordinatesOutOfBounds extends GameError
    case object InvalidMatch           extends GameError
    case object PlayerNotInRoom        extends GameError
  }

  implicit val codec: Codec[GameState] = {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("type")
    deriveConfiguredCodec
  }

}
