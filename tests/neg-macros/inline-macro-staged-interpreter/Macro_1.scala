
import scala.quoted._
import scala.quoted.autolift._

object E {

  inline def eval[T](inline x: E[T]): T = ${ impl(x) }

  def impl[T](x: E[T]): Staged[T] = x.lift

}

trait E[T] {
  def lift: Staged[T]
}

case class I(n: Int) extends E[Int] {
  def lift: Staged[Int] = n
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift: Staged[T] = op(x.lift, y.lift)
}

trait Op2[T] {
  def apply(x: Expr[T], y: Expr[T]): Staged[T]
}

trait Plus2[T] extends Op2[T]
object Plus2 {
  implicit case object IPlus extends Plus2[Int] {
    def apply(x: Expr[Int], y: Expr[Int]): Staged[Int] = '{$x + $y}
  }
}
