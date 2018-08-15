import quoted._

object Macro_1 {
  transparent def foo(b: Boolean & Constant): Unit = ~fooImpl(b)
  def fooImpl(b: Boolean): Expr[Unit] =
    if (b) '(println("foo(true)"))
    else QuoteError("foo cannot be called with false")
}
