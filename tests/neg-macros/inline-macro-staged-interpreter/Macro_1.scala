
import scala.quoted._
import scala.quoted.autolift.{given _}
import scala.quoted.matching._

object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T: Type](x: Expr[E[T]]) with QuoteContext : Expr[T] = x.value.lift

  implicit def ev1[T: Type]: ValueOfExpr[E[T]] = new ValueOfExpr {
    def apply(x: Expr[E[T]]) with QuoteContext : Option[E[T]] = x match {
      case '{ I(${Const(n)}) } => Some(I(n).asInstanceOf[E[T]])
      case '{ Plus[T](${Value(x)}, ${Value(y)})(given $op) } if op.matches('{Plus2.IPlus}) => Some(Plus(x, y)(given Plus2.IPlus.asInstanceOf[Plus2[T]]).asInstanceOf[E[T]])
      case _ => None
    }
  }

  object Value {
    def unapply[T, U >: T](expr: Expr[T])(given ValueOfExpr[U], QuoteContext): Option[U] = expr.getValue
  }
}

trait E[T] {
  def lift with QuoteContext : Expr[T]
}

case class I(n: Int) extends E[Int] {
  def lift with QuoteContext : Expr[Int] = n
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift with QuoteContext : Expr[T] = op(x.lift, y.lift)
}

trait Op2[T] {
  def apply(x: Expr[T], y: Expr[T]) with QuoteContext : Expr[T]
}

trait Plus2[T] extends Op2[T]
object Plus2 {
  implicit case object IPlus extends Plus2[Int] {
    def apply(x: Expr[Int], y: Expr[Int]) with QuoteContext : Expr[Int] = '{$x + $y}
  }
}
