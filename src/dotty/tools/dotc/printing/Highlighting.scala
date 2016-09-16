package dotty.tools
package dotc
package printing

import scala.collection.mutable

object Highlighting {

  implicit def highlightToString(h: Highlight): String = h.toString
  implicit def hbufToString(hb: HighlightBuffer): String = hb.toString

  abstract class Highlight(private val highlight: String) {
    def text: String

    override def toString = highlight + text + Console.RESET

    def +(other: Highlight): HighlightBuffer =
      new HighlightBuffer(this) + other

    def +(other: String): HighlightBuffer =
      new HighlightBuffer(this) + other
  }

  abstract class Modifier(private val mod: String, text: String) extends Highlight(Console.RESET) {
    override def toString =
      mod + super.toString
  }

  case class HighlightBuffer(hl: Highlight) {
    val buffer = new mutable.ListBuffer[String]

    buffer += hl.toString

    def +(other: Highlight): HighlightBuffer = {
      buffer += other.toString
      this
    }

    def +(other: String): HighlightBuffer = {
      buffer += other
      this
    }

    override def toString =
      buffer.mkString
  }

  case class Red(text: String) extends Highlight(Console.RED)
  case class Blue(text: String) extends Highlight(Console.BLUE)
  case class Cyan(text: String) extends Highlight(Console.CYAN)
  case class Black(text: String) extends Highlight(Console.BLACK)
  case class Green(text: String) extends Highlight(Console.GREEN)
  case class White(text: String) extends Highlight(Console.WHITE)
  case class Yellow(text: String) extends Highlight(Console.YELLOW)
  case class Magenta(text: String) extends Highlight(Console.MAGENTA)

  case class RedB(text: String) extends Highlight(Console.RED_B)
  case class BlueB(text: String) extends Highlight(Console.BLUE_B)
  case class CyanB(text: String) extends Highlight(Console.CYAN_B)
  case class BlackB(text: String) extends Highlight(Console.BLACK_B)
  case class GreenB(text: String) extends Highlight(Console.GREEN_B)
  case class WhiteB(text: String) extends Highlight(Console.WHITE_B)
  case class YellowB(text: String) extends Highlight(Console.YELLOW_B)
  case class MagentaB(text: String) extends Highlight(Console.MAGENTA_B)

  case class Bold(text: String) extends Modifier(Console.BOLD, text)
  case class Underlined(text: String) extends Modifier(Console.UNDERLINED, text)
}
