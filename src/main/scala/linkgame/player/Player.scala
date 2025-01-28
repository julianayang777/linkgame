package linkgame.player

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class Player(name: String)

object Player {
  implicit val codec: Codec[Player] = deriveCodec
}
