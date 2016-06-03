sealed abstract class TA
sealed abstract class TB extends TA
case object B extends TB
case object B2 extends TB

case class CC(i: Int, tb: TB)

object Test {
  def foo: PartialFunction[CC, Unit] = {
    case CC(_, B) => ()
  }
}