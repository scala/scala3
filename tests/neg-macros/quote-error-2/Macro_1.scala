import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${ fooImpl('b) }
  def fooImpl(using s: Scope)(b: s.Expr[Boolean]): s.Expr[Unit] =
    '{println(${msg(b.unliftOrError)})}

  def msg(using s: Scope)(b: Boolean): s.Expr[String] =
    if (b) '{"foo(true)"}
    else { report.error("foo cannot be called with false"); '{ ??? } }

}
