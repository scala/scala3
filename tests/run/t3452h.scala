class Mix extends Foo with Bar { f; }
trait T
abstract class Foo {
  type I = I1|Null
  class I1 extends T
  def f: I
  f
}
trait Bar {
  type I >: Null <: T|Null
  def f: I = null
  f
  def gobble: I = null
}

object Test extends dotty.runtime.LegacyApp {
  new Mix
}
