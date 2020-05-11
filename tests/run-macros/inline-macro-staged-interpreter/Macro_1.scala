
import scala.quoted._


object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T](using s: Scope)(expr: s.Expr[E[T]])(using s.Type[T]): s.Expr[T] =
    expr.unliftOrError.lift

  implicit def ev1[T](using s: Scope)(using s.Type[T]): s.Unliftable[E[T]] = new s.Unliftable { // TODO use type class derivation
    def fromExpr(x: s.Expr[E[T]]): Option[E[T]] = (x match {
      case '{ I(${Const(n)}) } => Some(I(n))
      case '{ D(${Const(n)}) } => Some(D(n))
      case '{ Plus[Int](${Value(x)}, ${Value(y)})(using $op) } => Some(Plus(x, y)(using Plus2.IPlus))
      case '{ Plus[Double](${Value(x)}, ${Value(y)})(using $op) } => Some(Plus(x, y)(using Plus2.DPlus))
      case '{ Times[Int](${Value(x)}, ${Value(y)})(using $op) } => Some(Times(x, y)(using Times2.ITimes))
      case '{ Times[Double](${Value(x)}, ${Value(y)})(using $op) } => Some(Times(x, y)(using Times2.DTimes))
      case _ => None
    }).asInstanceOf[Option[E[T]]]
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

case class D(n: Double) extends E[Double] {
  def lift(using s: Scope): s.Expr[Double] = Expr(n)
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift(using s: Scope): s.Expr[T] = op(x.lift, y.lift)
}

case class Times[T](x: E[T], y: E[T])(implicit op: Times2[T]) extends E[T] {
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

  implicit case object DPlus extends Plus2[Double] {
    def apply(using s: Scope)(x: s.Expr[Double], y: s.Expr[Double]): s.Expr[Double] = '{$x + $y}
  }
}

trait Times2[T] extends Op2[T]
object Times2 {
  implicit case object ITimes extends Times2[Int] {
    def apply(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{$x * $y}
  }

  implicit case object DTimes extends Times2[Double] {
    def apply(using s: Scope)(x: s.Expr[Double], y: s.Expr[Double]): s.Expr[Double] = '{$x * $y}
  }
}
