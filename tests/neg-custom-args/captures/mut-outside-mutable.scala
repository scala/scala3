import caps.Mutable

trait IterableOnce[T]:
  def iterator: Iterator[T]^{this}
  mut def foreach(op: T => Unit): Unit // error

trait Foo extends Mutable:
  def bar =
    mut def baz() = 1 // error
    baz()
