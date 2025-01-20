package linkgame

sealed trait GameLevel {

}

object GameLevel {

  case object Easy extends GameLevel
  case object Medium extends GameLevel
  case object Hard extends GameLevel

  def toEmoji(n: Int): String = {
    // TODO: Initial implementation, the emojis will vary on the GameLevel
    n match {
      case 1 => "\uD83D\uDD35" // 🔵
      case 2 => "\uD83D\uDFE1" // 🟡
      case 3 => "\uD83D\uDD34" // 🔴
      case 4 => "\uD83D\uDFE2" // 🟢
      case 5 => "\uD83D\uDFE3" // 🟣
      case 6 => "\uD83D\uDC3C" // 🐼
      case 7 => "\uD83D\uDC28" // 🐨
      case 8 => "\uD83D\uDC23" // 🐣
      case 9 => "\uD83E\uDD86" // 🦆
      case 10 => "\uD83D\uDC38" // 🐸
      case 11 => "\uD83E\uDD93" // 🦓
      case 12 => "\uD83E\uDD88" // 🦈
    }
  }

}
