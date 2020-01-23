import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl('b)}
  def fooImpl(b: Expr[Boolean]) with (qctx: QuoteContext) : Expr[Unit] =
    if (b.value) '{println("foo(true)")}
    else { qctx.error("foo cannot be called with false"); '{ ??? } }
}
