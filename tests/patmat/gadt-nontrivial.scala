object O {
  sealed trait Expr[T]
  case class BoolExpr(v: Boolean) extends Expr[Boolean]
  case class IntExpr(v: Int) extends Expr[Int]
  case class AddExpr(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]

  def join[T](e1: Expr[T], e2: Expr[T]): Expr[T] = (e1, e2) match {
    case (BoolExpr(b1), BoolExpr(b2)) => BoolExpr(b1 && b2)
    case (IntExpr(i1), IntExpr(i2)) => IntExpr(i1 + i2)
    case (AddExpr(ei1, ei2), ie) => join(join(ei1, ei2), ie)
  }
}
