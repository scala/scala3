package generic

import Shapes._

/** enum Color {
 *    case Red
 *    case Green
 *    case Blue
 *  }
 */
sealed trait Color extends Enum

object Color {

  private val $values = new runtime.EnumValues[Color]
  def valueOf = $values.fromInt
  def withName = $values.fromName
  def values = $values.values

  private def $new(tag: Int, name: String) = new Color {
    def ordinal = tag
    override def toString = name
    $values.register(this)
  }

  val Red: Color = $new(0, "Red")
  val Green: Color = $new(1, "Green")
  val Blue: Color = $new(2, "Blue")

  implicit val ColorShape: Color `shaped` EnumValue[Color] =
    new (Color `shaped` EnumValue[Color]) {
      def toShape(x: Color) = EnumValue(x.ordinal)
      def fromShape(x: EnumValue[Color]) = Color.valueOf(x.tag)
    }
}
