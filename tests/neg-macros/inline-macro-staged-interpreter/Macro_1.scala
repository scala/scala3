
import scala.quoted._


object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T: Staged](x: Expr[E[T]]) (using QuoteContext): Expr[T] = x.unliftOrError.lift

  implicit def ev1[T: Staged]: Unliftable[E[T]] = new Unliftable {
    def apply(x: Expr[E[T]]) (using QuoteContext): Option[E[T]] = x match {
      case '{ I(${Const(n)}) } => Some(I(n).asInstanceOf[E[T]])
      case '{ Plus[T](${Value(x)}, ${Value(y)})(using $op) } if op.matches('{Plus2.IPlus}) => Some(Plus(x, y)(using Plus2.IPlus.asInstanceOf[Plus2[T]]).asInstanceOf[E[T]])
      case _ => None
    }
  }

  object Value {
    def unapply[T, U >: T](expr: Expr[T])(using Unliftable[U], QuoteContext): Option[U] = expr.unlift
  }
}

trait E[T] {
  def lift (using QuoteContext): Expr[T]
}

case class I(n: Int) extends E[Int] {
  def lift (using QuoteContext): Expr[Int] = Expr(n)
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift (using QuoteContext): Expr[T] = op(x.lift, y.lift)
}

trait Op2[T] {
  def apply(x: Expr[T], y: Expr[T]) (using QuoteContext): Expr[T]
}

trait Plus2[T] extends Op2[T]
object Plus2 {
  implicit case object IPlus extends Plus2[Int] {
    def apply(x: Expr[Int], y: Expr[Int]) (using QuoteContext): Expr[Int] = '{$x + $y}
  }
}
