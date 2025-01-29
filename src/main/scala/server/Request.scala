package server

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.generic.semiauto.deriveDecoder
import linkgame.game.Coordinate

sealed trait Request

object Request {
  final case class Match(p1: Coordinate, p2: Coordinate) extends Request

  object Match {
    implicit val decoder: Decoder[Match] = deriveDecoder
  }

  implicit val decoder: Decoder[Request] = {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("type")
    deriveConfiguredDecoder
  }
}
