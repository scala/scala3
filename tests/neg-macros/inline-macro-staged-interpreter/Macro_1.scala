
import scala.quoted.*


object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T: Type](x: Expr[E[T]]) (using Quotes): Expr[T] = x.valueOrError.lift

  implicit def ev1[T: Type]: FromExpr[E[T]] = new FromExpr {
    def unapply(x: Expr[E[T]])(using Quotes) = x match {
      case '{ I(${Expr(n)}) } => Some(I(n).asInstanceOf[E[T]])
      case '{ Plus[T](${Value(x)}, ${Value(y)})(using $op) } if op.matches('{Plus2.IPlus}) => Some(Plus(x, y)(using Plus2.IPlus.asInstanceOf[Plus2[T]]).asInstanceOf[E[T]])
      case _ => None
    }
  }

  object Value {
    def unapply[T](expr: Expr[T])(using FromExpr[T], Quotes): Option[T] = expr.value
  }
}

trait E[T] {
  def lift (using Quotes): Expr[T]
}

case class I(n: Int) extends E[Int] {
  def lift (using Quotes): Expr[Int] = Expr(n)
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift (using Quotes): Expr[T] = op(x.lift, y.lift)
}

trait Op2[T] {
  def apply(x: Expr[T], y: Expr[T]) (using Quotes): Expr[T]
}

trait Plus2[T] extends Op2[T]
object Plus2 {
  implicit case object IPlus extends Plus2[Int] {
    def apply(x: Expr[Int], y: Expr[Int]) (using Quotes): Expr[Int] = '{$x + $y}
  }
}
