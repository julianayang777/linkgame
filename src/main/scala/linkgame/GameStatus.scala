package linkgame

sealed trait GameStatus

object GameStatus {
  final case object InProgress extends GameStatus
  final case object Finished   extends GameStatus
}
