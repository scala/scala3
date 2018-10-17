import quoted._

object Macro_1 {
  inline def foo(inline b: Boolean): Unit = ${fooImpl(b)}
  def fooImpl(b: Boolean): Staged[Unit] =
    if (b) '{println("foo(true)")}
    else ???
}
