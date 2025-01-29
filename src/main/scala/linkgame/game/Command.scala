package linkgame.game

import linkgame.player.Player

sealed trait Command

object Command {
  final case class Join(player: Player)                  extends Command
  final case class Match(player: Player, p1: Coordinate, p2: Coordinate) extends Command
}
