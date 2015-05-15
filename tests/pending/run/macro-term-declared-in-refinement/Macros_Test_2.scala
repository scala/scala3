import scala.language.reflectiveCalls

class Base

object Test extends dotty.runtime.LegacyApp {
  val macros = new Base { def foo: Unit = macro Impls.foo }
  macros.foo
}
