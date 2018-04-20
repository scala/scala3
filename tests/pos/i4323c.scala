sealed trait Expr[T] { outer =>
  class Inner {
    def join(other: Expr[T]): Expr[T] = (outer, other) match {
      case (IExpr(i1), IExpr(i2)) => IExpr(i1 + i2)
      case (BExpr(b1), BExpr(b2)) => BExpr(b1 & b2)
    }
  }
}

case class IExpr(value: Int) extends Expr[Int]
case class BExpr(value: Boolean) extends Expr[Boolean]
