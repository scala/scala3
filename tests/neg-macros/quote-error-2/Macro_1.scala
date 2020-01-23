import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${ fooImpl(b) }
  def fooImpl(b: Boolean) with QuoteContext : Expr[Unit] =
    '{println(${msg(b)})}

  def msg(b: Boolean) with (qctx: QuoteContext) : Expr[String] =
    if (b) '{"foo(true)"}
    else { qctx.error("foo cannot be called with false"); '{ ??? } }

}
