package server

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import linkgame.game.GameState.{getGameStatus, playerInGame}
import linkgame.game.{GameLevel, GameState}
import linkgame.player.Player

import java.util.UUID

final case class GameRoomResponse(
  id: UUID,
  name: String,
  level: GameLevel,
  joinedPlayers: Int,
  requiredPlayers: Int,
  status: String,
  wasInRoom: Boolean,
) {

  implicit val encoder: Encoder[GameRoomResponse] = deriveEncoder
  implicit val decoder: Decoder[GameRoomResponse] = deriveDecoder
}

object GameRoomResponse {
  def fromGameState(id: UUID, player: Player, name: String, state: GameState): GameRoomResponse = {
    val (level, joinedPlayers, requiredPlayers) = state match {
      case GameState.AwaitingPlayers(level, requiredPlayers, players) => (level, players.size, requiredPlayers)
      case GameState.GameStartsSoon(level, players, _)                => (level, players.size, players.size)
      case GameState.InProgress(level, playerBoards, _)               => (level, playerBoards.size, playerBoards.size)
      case GameState.Win(level, _, players, _)                        => (level, players.size, players.size)
    }
    GameRoomResponse(id, name, level, joinedPlayers, requiredPlayers, getGameStatus(state), playerInGame(player, state))
  }
}
