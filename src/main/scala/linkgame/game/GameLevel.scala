package linkgame.game

sealed trait GameLevel

object GameLevel {

  case object Easy   extends GameLevel
  case object Medium extends GameLevel
  case object Hard   extends GameLevel

  def unapply(level: String): Option[GameLevel] = level.toLowerCase match {
    case "easy"   => Some(Easy)
    case "medium" => Some(Medium)
    case "hard"   => Some(Hard)
    case _        => None
  }
}
