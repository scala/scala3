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

class NotStateful:
  update class C // error
  class D:
    update def foo() = () // error

class Outer extends Stateful:
  update object O // error
  update case class C(x: Int) // error
