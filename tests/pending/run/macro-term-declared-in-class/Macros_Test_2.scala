class Macros {
  def foo: Unit = macro Impls.foo
}

object Test extends dotty.runtime.LegacyApp {
  new Macros().foo
}