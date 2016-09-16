package dotty.tools
package dotc
package printing

import scala.collection.mutable

object Highlighting {

  implicit def colorToString(c: Color): String = c.toString
  implicit def cbufToString(cb: ColorBuffer): String = cb.toString

  abstract class Color(private val color: String) {
    def text: String

    override def toString = color + text + Console.RESET

    def +(other: Color): ColorBuffer =
      new ColorBuffer(this) + other

    def +(other: String): ColorBuffer =
      new ColorBuffer(this) + other
  }

  case class ColorBuffer(color: Color) {
    val buffer = new mutable.ListBuffer[String]

    buffer += color.toString

    def +(color: Color): ColorBuffer = {
      buffer += color.toString
      this
    }

    def +(str: String): ColorBuffer = {
      buffer += str
      this
    }

    override def toString =
      buffer.mkString
  }

  case class Red(text: String) extends Color(Console.RED)
  case class Blue(text: String) extends Color(Console.BLUE)
  case class Cyan(text: String) extends Color(Console.CYAN)
  case class Black(text: String) extends Color(Console.BLACK)
  case class Green(text: String) extends Color(Console.GREEN)
  case class White(text: String) extends Color(Console.WHITE)
  case class Yellow(text: String) extends Color(Console.YELLOW)
  case class Magenta(text: String) extends Color(Console.MAGENTA)

  case class RedB(text: String) extends Color(Console.RED_B)
  case class BlueB(text: String) extends Color(Console.BLUE_B)
  case class CyanB(text: String) extends Color(Console.CYAN_B)
  case class BlackB(text: String) extends Color(Console.BLACK_B)
  case class GreenB(text: String) extends Color(Console.GREEN_B)
  case class WhiteB(text: String) extends Color(Console.WHITE_B)
  case class YellowB(text: String) extends Color(Console.YELLOW_B)
  case class MagentaB(text: String) extends Color(Console.MAGENTA_B)
}
