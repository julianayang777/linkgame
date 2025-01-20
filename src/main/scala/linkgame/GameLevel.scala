package linkgame

sealed trait GameLevel

object GameLevel {

  case object Easy   extends GameLevel
  case object Medium extends GameLevel
  case object Hard   extends GameLevel

}
