import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${ fooImpl(b) }
  def fooImpl(b: Boolean): Staged[Unit] =
    '{println(${msg(b)})}

  def msg(b: Boolean): Staged[String] =
    if (b) '{"foo(true)"}
    else QuoteError("foo cannot be called with false")

}
