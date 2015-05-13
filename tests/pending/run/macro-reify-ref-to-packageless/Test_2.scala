object Test extends dotty.runtime.LegacyApp {
  def foo: Int = macro Impls.foo
  println(foo)
}