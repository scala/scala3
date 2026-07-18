import caps.Stateful

trait IterableOnce[T]:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit // error

trait Foo extends Stateful:
  def bar =
    update def baz() = 1 // error
    baz()

trait Bar extends Stateful:
  class Baz:
    update def baz() = 1 // error
