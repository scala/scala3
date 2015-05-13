object Test extends dotty.runtime.LegacyApp {
  def foo(bar: String = { def foo: String = macro Impls.foo; foo }) = println(bar)

  foo()
  foo("it works")
  foo()
}