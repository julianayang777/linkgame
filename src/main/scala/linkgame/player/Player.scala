package linkgame.player

import io.circe.{Codec, KeyDecoder, KeyEncoder}
import io.circe.generic.semiauto.deriveCodec

final case class Player(name: String)

object Player {
  type PlayerId = String

  implicit val codec: Codec[Player] = deriveCodec
  implicit val playerKeyDecoder: KeyDecoder[Player] = KeyDecoder[String].map(Player.apply)
  implicit val playerKeyEncoder: KeyEncoder[Player] = KeyEncoder[String].contramap(_.name)
}
