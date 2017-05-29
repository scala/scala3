sealed trait Expr
class IntExpr extends Expr
class BooleanExpr extends Expr

object IntExpr {
  def unapply(expr: Expr): Option[IntExpr] = ???
}

object BooleanExpr {
  def unapply(expr: Expr): Option[BooleanExpr] = ???
}


class Test {
  def foo(x: List[Expr]): Int = x match {
    case IntExpr(_) :: xs => 1
    case BooleanExpr(_) :: xs => 1
    case Nil => 2
  }

  def bar(x: Expr): Int = x match {
    case IntExpr(_) => 1
    case BooleanExpr(_) => 2
  }
}
