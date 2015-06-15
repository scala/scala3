class Macros {
  class Macros {
    def foo: Unit = macro Impls.foo
  }
}

object Test extends dotty.runtime.LegacyApp {
  val outer = new Macros()
  new outer.Macros().foo
}