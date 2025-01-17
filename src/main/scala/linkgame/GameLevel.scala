package linkgame

object GameLevel {

  def toEmoji(n: Int): String = {
    // TODO: Initial implementation, the emojis will vary on the GameLevel
    n match {
      case 1 => "\uD83D\uDD34"
      case 2 => "\uD83D\uDFE0"
      case 3 => "\uD83D\uDFE1"
      case 4 => "\uD83D\uDFE2"
      case 5 => "\uD83D\uDD35"
      case 6 => "\uD83D\uDFE3"
      case 7 => "\uD83D\uDE00"
      case 8 => "\uD83D\uDFE7"
      case _ => "\uD83D\uDF90"
    }
  }

}
