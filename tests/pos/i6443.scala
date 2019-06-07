object Test {
  trait Expr { type T }
  type ExprExact[A] = Expr { type T = A }
  type IndirectExprExact[A] = Expr { type S = A; type T = S }

  def foo[A](e: ExprExact[A]): Unit = e match {
    case _: IndirectExprExact[Int] =>
  }
}