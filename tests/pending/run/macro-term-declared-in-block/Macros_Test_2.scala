object Test extends dotty.runtime.LegacyApp {
  {
    def foo: Unit = macro Impls.foo
    foo
  }
}