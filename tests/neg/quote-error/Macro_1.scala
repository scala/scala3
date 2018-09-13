import quoted._

object Macro_1 {
  inline def foo(transparent b: Boolean): Unit = ~fooImpl(b)
  def fooImpl(b: Boolean): Expr[Unit] =
    if (b) '(println("foo(true)"))
    else QuoteError("foo cannot be called with false")
}
