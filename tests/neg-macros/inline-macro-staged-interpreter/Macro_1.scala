
import scala.quoted._


object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T](using s: Scope)(x: s.Expr[E[T]])(using s.Type[T]): s.Expr[T] = x.unliftOrError.lift

  implicit def ev1[T](using s: Scope)(using s.Type[T]): s.Unliftable[E[T]] = new s.Unliftable {
    def fromExpr(x: s.Expr[E[T]]): Option[E[T]] = x match {
      case '{ I(${Const(n)}) } => Some(I(n).asInstanceOf[E[T]])
      case '{ Plus[T](${Value(x)}, ${Value(y)})(using $op) } if op.matches('{Plus2.IPlus}) => Some(Plus(x, y)(using Plus2.IPlus.asInstanceOf[Plus2[T]]).asInstanceOf[E[T]])
      case _ => None
    }
  }

  object Value {
    def unapply[T](using s: Scope)(expr: s.Expr[T])(using s.Unliftable[T]): Option[T] = expr.unlift
  }
}

trait E[T] {
  def lift(using s: Scope): s.Expr[T]
}

case class I(n: Int) extends E[Int] {
  def lift(using s: Scope): s.Expr[Int] = Expr(n)
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift(using s: Scope): s.Expr[T] = op(x.lift, y.lift)
}

trait Op2[T] {
  def apply(using s: Scope)(x: s.Expr[T], y: s.Expr[T]): s.Expr[T]
}

trait Plus2[T] extends Op2[T]
object Plus2 {
  implicit case object IPlus extends Plus2[Int] {
    def apply(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{$x + $y}
  }
}
