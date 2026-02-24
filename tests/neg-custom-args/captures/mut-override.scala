import caps.Stateful

trait IterableOnce[T] extends Stateful:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit

trait Iterator[T] extends IterableOnce[T]:
  def iterator = this
  def hasNext: Boolean
  update def next(): T
  update def foreach(op: T => Unit): Unit = ???
  override update def toString = ???   // error

trait Iterable[T] extends IterableOnce[T]:
  def iterator: Iterator[T] = ???
  def foreach(op: T => Unit) = iterator.foreach(op)

trait BadIterator[T] extends Iterator[T]:
  override update def hasNext: Boolean  // error
