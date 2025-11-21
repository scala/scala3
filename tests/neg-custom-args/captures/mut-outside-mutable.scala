import caps.Mutable

trait IterableOnce[T]:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit // error

trait Foo extends Mutable:
  def bar =
    update def baz() = 1 // error
    baz()

trait Bar extends Mutable:
  class Baz:
    update def baz() = 1 // error
