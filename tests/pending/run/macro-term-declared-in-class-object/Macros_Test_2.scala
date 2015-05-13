class Macros {
  object Macros {
    def foo: Unit = macro Impls.foo
  }
}

object Test extends dotty.runtime.LegacyApp {
  val outer = new Macros()
  outer.Macros.foo
}