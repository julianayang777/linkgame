package linkgame.player

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, KeyDecoder, KeyEncoder}

import java.util.UUID

// TODO: Assume for now that name cannot be equal for two different players
final case class Player(name : String)

object Player {
  implicit val codec: Codec[Player]                 = deriveCodec
  implicit val playerKeyDecoder: KeyDecoder[Player] = KeyDecoder[String].map(Player.apply)
  implicit val playerKeyEncoder: KeyEncoder[Player] = KeyEncoder[String].contramap(_.name)
}
