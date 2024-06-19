sealed trait Exp[T]
case class Fun[A, B](f: Exp[A => B]) extends Exp[A => B]

class Test {
  def eval(e: Fun[Int, Int]) = e match {
    case Fun(x: Fun[Int, Double]) => ???                                   // warn: unchecked
    case Fun(x: Exp[Int => String]) => ??? // warn: unreachable            // warn: unchecked
    case _ =>                              // warn: unreachable-only-null
  }
}
