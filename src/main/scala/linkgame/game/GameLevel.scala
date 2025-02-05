package linkgame.game

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, KeyDecoder, KeyEncoder}

sealed trait GameLevel

object GameLevel {

  def unapply(level: String): Option[GameLevel] = level.toLowerCase match {
    case "easy"   => Some(Easy)
    case "medium" => Some(Medium)
    case "hard"   => Some(Hard)
    case _        => None
  }

  case object Easy   extends GameLevel
  case object Medium extends GameLevel
  case object Hard   extends GameLevel

  implicit val codec: Codec[GameLevel] = deriveCodec

  implicit val levelKeyDecoder: KeyDecoder[GameLevel] = KeyDecoder[String].map {
    case "easy"   => Easy
    case "medium" => Medium
    case "hard"   => Hard
    case _        => throw new IllegalArgumentException("Invalid game level")
  }

  implicit val levelKeyEncoder: KeyEncoder[GameLevel] = KeyEncoder[String].contramap {
    case Easy   => "easy"
    case Medium => "medium"
    case Hard   => "hard"
  }
}
