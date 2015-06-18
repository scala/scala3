trait Base {
  def foo: Unit = macro Impls.foo
}

object Macros extends Base

class Macros extends Base

object Test extends dotty.runtime.LegacyApp {
  (new Base {}).foo
  Macros.foo
  new Macros().foo
}