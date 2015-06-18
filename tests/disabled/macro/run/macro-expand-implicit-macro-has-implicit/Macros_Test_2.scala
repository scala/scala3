object Test extends dotty.runtime.LegacyApp {
  implicit val x = 42
  def foo(implicit x: Int): Unit = macro Impls.foo
  foo
}