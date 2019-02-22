import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${ fooImpl(b) }
  def fooImpl(b: Boolean): Expr[Unit] =
    '{println(${msg(b)})}

  def msg(b: Boolean): Expr[String] =
    if (b) '{"foo(true)"}
    else QuoteError("foo cannot be called with false")

}
