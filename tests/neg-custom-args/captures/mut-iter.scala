import caps.*

trait IterableOnce[+T] extends Stateful:
  def iterator: Iterator[T]^{this}
  update def foreach(op: T => Unit): Unit
  update def exists(op: T => Boolean): Boolean

trait Iterator[+T] extends IterableOnce[T]:
  def hasNext: Boolean
  update def next(): T
  update def foreach(op: T => Unit): Unit =
    while hasNext do op(next())
  update def exists(op: T => Boolean): Boolean /* = ... next() ... */

trait IterableBad[+T] extends IterableOnce[T]:
  def iterator: Iterator[T]^{this} /* = new Iterator[T] { ... } */
  def foreach(op: T => Unit) = iterator.foreach(op)  // error
  def exists(op: T => Boolean) = iterator.exists(op) // error

trait IterableBad2[+T] extends IterableOnce[T]:
  def iterator: Iterator[T]^ /* = new Iterator[T] { ... } */ // error
  def foreach(op: T => Unit) = iterator.foreach(op)
  def exists(op: T => Boolean) = iterator.exists(op)

trait IterableGood[+T] extends Stateful:
  def iterator: Iterator[T]^ /* = new Iterator[T] { ... } */
    // Creating a fresh iterator each time works, but it cannot override
    // IterableOnce's iterator
  def foreach(op: T => Unit) = iterator.foreach(op)
  def exists(op: T => Boolean) = iterator.exists(op)
