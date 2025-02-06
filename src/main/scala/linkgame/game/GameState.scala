package linkgame.game

import cats.effect.IO
import cats.implicits._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import linkgame.game.Board.{Board, deleteTileFromBoard, initBoard, isCoordinateOnBoard, isEmpty, isSolvable, isValidPath, printBoard, refreshBoard}
import linkgame.game.GameState.GameError
import linkgame.player.Player

import java.time.Instant
import scala.concurrent.duration.{DurationLong, FiniteDuration}

sealed trait GameState {
  private val countDownDuration: FiniteDuration = 5.seconds

  def join(player: Player): Either[GameError, IO[GameState]] =
    this match {
      case state @ GameState.AwaitingPlayers(gameLevel, requiredPlayers, players) =>
        val newPlayers = players + player
        if (newPlayers.size < requiredPlayers) state.copy(players = newPlayers).pure[IO].asRight
        else GameState.GameStartsSoon(gameLevel, newPlayers, countDownDuration).pure[IO].asRight
      case inProgress: GameState.InProgress              =>
        Either.cond(inProgress.playerBoards.contains(player), inProgress.pure[IO], GameError.GameAlreadyStarted)
      case startSoon: GameState.GameStartsSoon           =>
        Either.cond(startSoon.players.contains(player), startSoon.pure[IO], GameError.GameAlreadyStarted)
      case _: GameState.Win                              => GameError.GameAlreadyEnded.asLeft
    }

  def attemptMatch(player: Player, p1: Coordinate, p2: Coordinate): Either[GameError, IO[GameState]] = {
    this match {
      case inProgress @ GameState.InProgress(gameLevel, playerBoards, startInstant) =>
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
              end           <- IO.realTimeInstant
              completionTime = (end.toEpochMilli - startInstant.toEpochMilli).millis
            } yield GameState.Win(gameLevel, player, completionTime)
          } else {
            for {
              newBoard        <-
                if (isSolvable(updatedBoard2)) { IO.pure { updatedBoard2 } }
                else { refreshBoard(updatedBoard2) }
              newPlayersBoards = playerBoards.updated(player, newBoard)
            } yield inProgress.copy(playerBoards = newPlayersBoards)
          }
        }
      case _: GameState.GameStartsSoon                                              => GameError.GameNotStarted.asLeft
      case _: GameState.AwaitingPlayers                                             => GameError.GameNotStarted.asLeft
      case _: GameState.Win                                                         => GameError.GameAlreadyEnded.asLeft
    }
  }

  def startGame: Either[GameError, IO[GameState]] =
    this match {
      case GameState.GameStartsSoon(gameLevel, players, _) =>
        (for {
          start       <- IO.realTimeInstant
          board       <- initBoard(gameLevel)
          _           <- printBoard(board)
          playerBoards = players.map(_ -> board).toMap
        } yield GameState.InProgress(gameLevel, playerBoards, start)).asRight
      case _: GameState.Win                                => GameError.GameAlreadyEnded.asLeft
      case _: GameState.InProgress                         => GameError.GameNotStarted.asLeft
      case _: GameState.AwaitingPlayers                    => GameError.GameNotStarted.asLeft
    }
}

object GameState {

  final case class AwaitingPlayers(gameLevel: GameLevel, requiredPlayers: Int, players: Set[Player]) extends GameState

  final case class GameStartsSoon(gameLevel: GameLevel, players: Set[Player], startIn: FiniteDuration) extends GameState

  final case class InProgress(gameLevel: GameLevel, playerBoards: Map[Player, Board], startInstant: Instant)
      extends GameState

  final case class Win(gameLevel: GameLevel, winner: Player, completionTime: FiniteDuration) extends GameState

  implicit val finiteDurationEncoder: Encoder[FiniteDuration] = Encoder.encodeLong.contramap[FiniteDuration](_.toMillis)
  implicit val finiteDurationDecoder: Decoder[FiniteDuration] = Decoder.decodeLong.emap {
    // Do not accept negative durations
    d => Either.cond(d >= 0, d.millis, "Duration must be non-negative")
  }

  implicit val playerBoardMapCodec: Encoder[Map[Player, Board]]   = Encoder.encodeMap[Player, Board]
  implicit val playerBoardMapDecoder: Decoder[Map[Player, Board]] = Decoder.decodeMap[Player, Board]

  sealed trait GameError

  object GameError {
    case object GameAlreadyStarted     extends GameError
    case object GameAlreadyEnded       extends GameError
    case object GameNotStarted         extends GameError
    case object CoordinatesOutOfBounds extends GameError
    case object InvalidMatch           extends GameError
    case object PlayerNotInRoom        extends GameError
    case object InvalidRequest         extends GameError
  }

  implicit val codec: Codec[GameState] = {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("type")
    deriveConfiguredCodec
  }

}
