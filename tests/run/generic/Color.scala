package generic

import Shapes._

/** enum Color {
 *    case Red
 *    case Green
 *    case Blue
 *  }
 */
sealed trait Color extends Enum

object Color extends EnumValues[Color](3) {

  private def $new(tag: Int, name: String) = new Color {
    def enumTag = tag
    override def toString = name
    registerEnumValue(this)
  }

  val Red: Color = $new(0, "Red")
  val Green: Color = $new(1, "Green")
  val Blue: Color = $new(2, "Blue")

  implicit val ColorShape: Color `shaped` EnumValue[Color] =
    new (Color `shaped` EnumValue[Color]) {
      def toShape(x: Color) = EnumValue(x.enumTag)
      def fromShape(x: EnumValue[Color]) = Color.value(x.tag)
    }
}