import caps.{any, Stateful, SharedCapability}

trait Iterator[T] extends Stateful:
  def hasNext: Boolean
  update def next(): T

  update def foreach[U](f: T => Unit): Unit =
    while hasNext do f(next())

  def stupidForeach[U](f: T => Unit): Unit =
    while hasNext do f(next())  // error

  def sneakyForeach(f: T => Unit): Unit =
    val it = new Iterator[T]:
      def hasNext = Iterator.this.hasNext
      update def next() = Iterator.this.next() // error
    while it.hasNext do f(it.next())
end Iterator

def listIterator[T](xs: List[T]): Iterator[T]^ = new Iterator[T]:
  private var current = xs
  def hasNext = current.nonEmpty
  update def next() = xs.runtimeChecked match
    case x :: xs1 =>
      current = xs1
      x

class IO extends SharedCapability:
  def write(x: Any): Unit = ()

def test(io: IO) =
  def proc: Int => Unit = i => io.write(i)
  val f = () => listIterator(List(1, 2, 3)).sneakyForeach(proc)
