import caps.Mutable

trait IterableOnce[T] extends Mutable:
  def iterator: Iterator[T]^{this}
  mut def foreach(op: T => Unit): Unit

trait Iterator[T] extends IterableOnce[T]:
  def iterator = this
  def hasNext: Boolean
  mut def next(): T
  mut def foreach(op: T => Unit): Unit = ???
  override mut def toString = ???   // error

trait Iterable[T] extends IterableOnce[T]:
  def iterator: Iterator[T] = ???
  def foreach(op: T => Unit) = iterator.foreach(op)

trait BadIterator[T] extends Iterator[T]:
  override mut def hasNext: Boolean  // error
