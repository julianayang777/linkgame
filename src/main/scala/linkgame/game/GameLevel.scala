package linkgame.game

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

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
}
