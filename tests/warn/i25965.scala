sealed trait Expr1[T]
case class IntExpr1(x: Int) extends Expr1[Int]
case class BooleanExpr(b: Boolean) extends Expr1[Boolean]

sealed trait Expr2[T]
case class IntExpr2(x: Int) extends Expr2[Int]

def foo[T](x: Expr1[T], y: Expr2[T]) = (x, y) match {
  case (IntExpr1(_), IntExpr2(_)) =>
}

sealed trait Expr3[T]
case class IntExpr3(x: Int) extends Expr3[Int]
case class BoolExpr3(b: Boolean) extends Expr3[Boolean]

def bar[T](x: Expr1[T], y: Expr3[T]) = (x, y) match { // warn
  case (IntExpr1(_), IntExpr3(_)) =>
}
