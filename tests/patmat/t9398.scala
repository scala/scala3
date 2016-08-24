sealed abstract class TA
sealed abstract class TB extends TA
case object B extends TB
case object B2 extends TB

case class CC(i: Int, tb: TB)

object Test {
  // Should warn that CC(_, B2) isn't matched
  def foo: CC => Unit = {
    case CC(_, B) => ()
  }
}