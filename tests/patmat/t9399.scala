sealed abstract class TA
sealed abstract class TB extends TA
case object A extends TA
case object B extends TB

sealed trait C
case class CTA(id: Int, da: TA) extends C
case class CTB(id: Int, da: TB) extends C

object Test {
  val test: C => Unit = {
    case CTA(_, A) =>
    case CTA(_, B) =>
    case CTB(_, B) =>
  }
}
