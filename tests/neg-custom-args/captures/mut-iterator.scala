import caps.{any, Stateful, SharedCapability}

trait Iterator[T] extends Stateful:
  def hasNext: Boolean
  def next(): T

  def map[U](f: T => U): Iterator[U] = new Iterator: // error
    def hasNext = Iterator.this.hasNext
    def next() = f(Iterator.this.next()) // error
end Iterator

def listIterator[T](xs: List[T]) = new Iterator[T]:
  private var current = xs
  def hasNext = current.nonEmpty
  def next() = xs.runtimeChecked match
    case x :: xs1 =>
      current = xs1 // error
      x

def mappedIterator[T, U](it: Iterator[T]^, f: T => U): Iterator[U] = new Iterator: // error
  def hasNext = it.hasNext
  def next() = f(it.next()) // error

class IO extends SharedCapability:
  def write(x: Any): Unit = ()

def test(io: IO) =
  val proc: Int => Int = i => { io.write(i); i * i }
  listIterator(List(1, 2, 3)).map(proc)
  val roit: Iterator[Int]^{any.rd} = listIterator(List(1, 2, 3))
  val mapped = roit.map(proc)
  mapped.next()
