package server

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import linkgame.game.GameState

import java.util.UUID

final case class GameRoomResponse (id: UUID, state: GameState){
  implicit val encoder: Encoder[GameRoomResponse] = deriveEncoder
  implicit val decoder: Decoder[GameRoomResponse] = deriveDecoder
}
