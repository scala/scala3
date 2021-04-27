
import scala.quoted.*


object E {

  inline def eval[T](inline x: E[T]): T = ${ impl('x) }

  def impl[T: Type](expr: Expr[E[T]]) (using Quotes): Expr[T] =
    expr.valueOrError.lift

  implicit def ev1[T: Type]: FromExpr[E[T]] = new FromExpr { // TODO use type class derivation
    def unapply(x: Expr[E[T]])(using Quotes) = (x match {
      case '{ I(${Expr(n)}) } => Some(I(n))
      case '{ D(${Expr(n)}) } => Some(D(n))
      case '{ Plus[Int](${Value(x)}, ${Value(y)})(using $op) } => Some(Plus(x, y)(using Plus2.IPlus))
      case '{ Plus[Double](${Value(x)}, ${Value(y)})(using $op) } => Some(Plus(x, y)(using Plus2.DPlus))
      case '{ Times[Int](${Value(x)}, ${Value(y)})(using $op) } => Some(Times(x, y)(using Times2.ITimes))
      case '{ Times[Double](${Value(x)}, ${Value(y)})(using $op) } => Some(Times(x, y)(using Times2.DTimes))
      case _ => None
    }).asInstanceOf[Option[E[T]]]
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

case class D(n: Double) extends E[Double] {
  def lift (using Quotes): Expr[Double] = Expr(n)
}

case class Plus[T](x: E[T], y: E[T])(implicit op: Plus2[T]) extends E[T] {
  def lift (using Quotes): Expr[T] = op(x.lift, y.lift)
}

case class Times[T](x: E[T], y: E[T])(implicit op: Times2[T]) extends E[T] {
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

  implicit case object DPlus extends Plus2[Double] {
    def apply(x: Expr[Double], y: Expr[Double]) (using Quotes): Expr[Double] = '{$x + $y}
  }
}

trait Times2[T] extends Op2[T]
object Times2 {
  implicit case object ITimes extends Times2[Int] {
    def apply(x: Expr[Int], y: Expr[Int]) (using Quotes): Expr[Int] = '{$x * $y}
  }

  implicit case object DTimes extends Times2[Double] {
    def apply(x: Expr[Double], y: Expr[Double]) (using Quotes): Expr[Double] = '{$x * $y}
  }
}
