object Test extends dotty.runtime.LegacyApp {
  def foo[U]: Unit = macro Impls.foo[U]
  foo[Int]
}