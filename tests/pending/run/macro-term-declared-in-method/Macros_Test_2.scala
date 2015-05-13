object Test extends dotty.runtime.LegacyApp {
  def bar() = {
    def foo: Unit = macro Impls.foo
    foo
  }

  bar()
}