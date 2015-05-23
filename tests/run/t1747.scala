object Foo extends Foo {
  def f: Unit = {}
}
class Foo

object Test extends dotty.runtime.LegacyApp { Foo }
