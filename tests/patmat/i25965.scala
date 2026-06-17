sealed trait Expr1[T]
case class IntExpr1(x: Int) extends Expr1[Int]
case class BooleanExpr(b: Boolean) extends Expr1[Boolean]

sealed trait Expr2[T]
case class IntExpr2(x: Int) extends Expr2[Int]

def foo[T](x: Expr1[T], y: Expr2[T]) = (x, y) match {
  case (IntExpr1(_), IntExpr2(_)) =>
}