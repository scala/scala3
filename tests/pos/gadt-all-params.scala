object `gadt-all-params` {
  enum Expr[T] {
    case UnitLit extends Expr[Unit]
  }

  def foo[T >: TT <: TT, TT](e: Expr[T]): T = e match {
    case Expr.UnitLit => ()
  }
}
