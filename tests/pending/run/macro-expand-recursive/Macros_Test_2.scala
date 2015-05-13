object Macros {
  def foo: Unit = macro Impls.foo
  def fooFoo: Unit = macro Impls.fooFoo
}

object Test extends dotty.runtime.LegacyApp {
  Macros.fooFoo
}