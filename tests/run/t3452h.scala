class Mix extends Foo with Bar { f; }
trait T
abstract class Foo {
  class I extends T
  def f: I
  f
}
trait Bar {
  type I >: Null <: T
  def f: I = null
  f
  def gobble: I = null
}

object Test extends App {
  new Mix
}
