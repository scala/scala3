import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl('b)}
  def fooImpl(using s: Scope)(b: s.Expr[Boolean]): s.Expr[Unit] =
    if (b.unliftOrError) '{println("foo(true)")}
    else ???
}
