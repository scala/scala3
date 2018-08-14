import quoted._

object Macro_1 {
  transparent def foo(b: Boolean & Constant): Unit = ~fooImpl(b)
  def fooImpl(b: Boolean): Expr[Unit] =
    '(println(~msg(b)))

  def msg(b: Boolean): Expr[String] =
    if (b) '("foo(true)")
    else QuoteError("foo cannot be called with false")

}
