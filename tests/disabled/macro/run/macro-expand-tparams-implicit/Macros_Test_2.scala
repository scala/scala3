object Test extends dotty.runtime.LegacyApp {
  def foo[U](x: U): Unit = macro Impls.foo[U]
  foo(42)
  foo("42")
}