import caps.{Stateful, SharedCapability}

trait Iterator[T] extends Stateful:
  def hasNext: Boolean
  update def next(): T

  def map[U](f: T => U): Iterator[U] = new Iterator:  // error
    def hasNext = Iterator.this.hasNext
    update def next() = f(Iterator.this.next()) // error
end Iterator

def listIterator[T](xs: List[T]) = new Iterator[T]:
  private var current = xs
  def hasNext = current.nonEmpty
  update def next() = xs.runtimeChecked match
    case x :: xs1 =>
      current = xs1
      x

def mappedIterator[T, U](it: Iterator[T]^, f: T => U): Iterator[U] = new Iterator: // error
  def hasNext = it.hasNext
  update def next() = f(it.next())

class IO extends SharedCapability:
  def write(x: Any): Unit = ()

def test(io: IO) =
  listIterator(List(1, 2, 3)).map: i =>
    io.write(i)
    i * i
