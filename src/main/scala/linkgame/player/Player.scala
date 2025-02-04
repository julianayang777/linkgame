package linkgame.player

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, KeyDecoder, KeyEncoder}

final case class Player(name : String)

object Player {
  implicit val codec: Codec[Player]                 = deriveCodec
  implicit val playerKeyDecoder: KeyDecoder[Player] = KeyDecoder[String].map(Player.apply)
  implicit val playerKeyEncoder: KeyEncoder[Player] = KeyEncoder[String].contramap(_.name)
}
