
object Macro {
  transparent def foo[X](x: X): Unit = ~fooImpl('(x))
  def fooImpl[X: quoted.Type](x: quoted.Expr[X]): quoted.Expr[Unit] = '()
}
