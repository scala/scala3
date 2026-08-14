sealed trait Expr1[T]
case class IntExpr1(x: Int) extends Expr1[Int]
case class BooleanExpr(b: Boolean) extends Expr1[Boolean]

sealed trait Expr2[T]
case class IntExpr2(x: Int) extends Expr2[Int]

case class Wrap[T](x: Expr1[T], y: Expr2[T])

def foo[T](w: Wrap[T]) = w match {
  case Wrap(IntExpr1(_), IntExpr2(_)) =>
}
