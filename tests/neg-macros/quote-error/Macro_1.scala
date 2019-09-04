import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl(b)}
  def fooImpl(b: Boolean) given (qctx: QuoteContext): Expr[Unit] =
    if (b) '{println("foo(true)")}
    else { qctx.error("foo cannot be called with false"); '{ ??? } }
}
