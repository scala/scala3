trait AxisCompanion {
   sealed trait Format
   object Format {
      case object Decimal extends Format
      case object Integer extends Format
   }
}
object Axis extends AxisCompanion
class Axis {
   import Axis.*
   def test( f: Format ) = f match {
      case Format.Integer => "Int"
   }
}
