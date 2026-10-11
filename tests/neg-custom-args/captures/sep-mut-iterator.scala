import caps.*

trait Iterator[T] extends Stateful, ExclusiveCapability:
  def hasNext: Boolean
  def next(): T

  def map[U](f: T => U): Iterator[U] = new Iterator: // error
    def hasNext = Iterator.this.hasNext
    def next() = ???
end Iterator

def mappedIterator[T, U](it: Iterator[T]^, f: T => U): Iterator[U] = new Iterator: // error
  def hasNext = it.hasNext
  def next() = ???

