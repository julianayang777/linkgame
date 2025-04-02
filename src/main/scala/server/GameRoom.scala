package server

import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import fs2.concurrent.Topic
import linkgame.game.Board.Path
import linkgame.game.GameState.GameError.InvalidRequest
import linkgame.game.{Command, GameState}
import org.http4s.websocket.WebSocketFrame

final case class GameRoom(state: GameState, topic: Topic[IO, WebSocketFrame.Text]) {
  def handleCommand(c: Command): IO[(GameRoom, Either[GameState.GameError, (Option[Path], GameState)])] = {
    def addNone(result: Either[GameState.GameError, IO[GameState]]): Either[GameState.GameError, IO[(Option[Path], GameState)]] =
      result.map(io => io.map(game => (None, game)))

    val newStateOrError: Either[GameState.GameError, IO[(Option[Path], GameState)]] = c match {
      case Command.Join(player)          => addNone(state.join(player))
      case Command.Match(player, p1, p2) => state.attemptMatch(player, p1, p2)
      case Command.StartGame             => addNone(state.startGame)
      case Command.InvalidCommand        => InvalidRequest.asLeft
    }
    newStateOrError match {
      case Left(error)     => IO.pure { (this, error.asLeft) }
      case Right(newState) => newState.flatMap {
        case result @ (_, state) => IO.pure { (copy(state = state), result.asRight) } }
    }
  }
}
