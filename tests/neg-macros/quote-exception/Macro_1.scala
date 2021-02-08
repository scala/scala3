import quoted.*

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl('b)}
  def fooImpl(b: Expr[Boolean]) (using Quotes): Expr[Unit] =
    if (b.valueOrError) '{println("foo(true)")}
    else ???
}
