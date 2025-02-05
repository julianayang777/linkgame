package server

import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import fs2.concurrent.Topic
import linkgame.game.GameState.GameError.InvalidRequest
import linkgame.game.{Command, GameState}
import org.http4s.websocket.WebSocketFrame

final case class GameRoom(state: GameState, topic: Topic[IO, WebSocketFrame.Text]) {
  def handleCommand(c: Command): IO[(GameRoom, Either[GameState.GameError, GameState])] = {
    val newStateOrError: Either[GameState.GameError, IO[GameState]] = c match {
      case Command.Join(player)          => state.join(player)
      case Command.Match(player, p1, p2) => state.attemptMatch(player, p1, p2)
      case Command.StartGame             => state.startGame
      case Command.InvalidCommand        => InvalidRequest.asLeft
    }
    newStateOrError match {
      case Left(error)     => IO.pure { (this, error.asLeft) }
      case Right(newState) => newState.flatMap { state => IO.pure { (copy(state = state), state.asRight) } }
    }
  }
}
