object Macros {
  def foo: Unit = macro Impls.foo[String]
}

object Test extends dotty.runtime.LegacyApp {
  import Macros._
  foo
}