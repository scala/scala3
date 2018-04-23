enum Expr[T] {
  case IExpr(value: Int) extends Expr[Int]
  case BExpr(value: Boolean) extends Expr[Boolean]


  def join[S](other: Expr[S]): Expr[T] = (this, other) match {
    case (IExpr(i1), IExpr(i2)) => IExpr(i1 + i2)
    case (IExpr(i), BExpr(b)) => IExpr(i * i)
    case (BExpr(b1), BExpr(b2)) => BExpr(b1 & b2)
    case (BExpr(b), IExpr(i)) => BExpr(b && i > 10)
  }
}
