object Foo {
  def f()()(implicit ctx: String): Int = ???
  def at[T](op: () => T): Unit = ()
  at(() => f()) // error: missing ()
}
