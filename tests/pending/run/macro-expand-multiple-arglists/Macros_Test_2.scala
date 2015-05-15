object Test extends dotty.runtime.LegacyApp {
  def foo(x: Int)(y: Int): Unit = macro Impls.foo
  foo(40)(2)
}