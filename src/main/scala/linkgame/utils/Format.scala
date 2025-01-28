package linkgame.utils

object Format {
  private val styles: Map[String, String] = Map(
    "RED"   -> "\u001B[31m",
    "GREEN" -> "\u001B[32m",
    "RESET" -> "\u001B[0m",
    "BOLD"  -> "\u001B[1m",
  )

  private def applyStyle(text: String, style: String): String = {
    val styleCode = styles.getOrElse(style.toUpperCase, styles("RESET"))
    s"$styleCode$text${styles("RESET")}"
  }

  def Bold(text: String): String  = applyStyle(text, "BOLD")
  def Red(text: String): String   = applyStyle(text, "RED")
  def Green(text: String): String = applyStyle(text, "GREEN")
}
