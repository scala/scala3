object O {
  sealed trait Expr[T]
  case class BExpr(bool: Boolean) extends Expr[Boolean]
  case class IExpr(int: Int) extends Expr[Int]

  def join[T](e1: Expr[T], e2: Expr[T]): Expr[T] = (e1, e2) match {
    case (IExpr(i1), IExpr(i2)) => IExpr(i1 + i2)
    case (BExpr(b1), BExpr(b2)) => BExpr(b1 & b2)
  }
}