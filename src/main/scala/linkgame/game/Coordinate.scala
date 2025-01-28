package linkgame.game

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class Coordinate(row: Int, column: Int)

object Coordinate {
  implicit val codec: Codec[Coordinate] = deriveCodec
}
