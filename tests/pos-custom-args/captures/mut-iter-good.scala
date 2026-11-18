import caps.*

trait IterableOnce[+T] extends Stateful:
  update def iterator: Iterator[T]^{this, any}
  update def foreach(op: T => Unit): Unit = iterator.foreach(op)
  update def exists(op: T => Boolean): Boolean = iterator.exists(op)

trait Iterator[+T] extends IterableOnce[T]:
  def hasNext: Boolean
  update def next(): T

  update def iterator: Iterator[T]^{this} = this
  override update def foreach(op: T => Unit): Unit =
    while hasNext do op(next())
  override update def exists(op: T => Boolean): Boolean =
    @annotation.tailrec def loop: Boolean =
      if hasNext then
        if op(next()) then true
        else loop
      else false
    loop

trait Iterable[+T] extends IterableOnce[T]:
  def iterator: Iterator[T]^{this, any}
  override def foreach(op: T => Unit): Unit = iterator.foreach(op)
  override def exists(op: T => Boolean): Boolean = iterator.exists(op)

class Counter(from: Int, to: Int) extends Iterator[Int], Mutable:
  var count = from
  def hasNext = count <= to
  update def next() =
    val res = count
    count += 1
    res

class Counters(from: Int, to: Int) extends Iterable[Int]:
  def iterator: Iterator[Int]^{any} = new Counter(from, to)

// Pretends that this runs in parallel. `f` and `g` are required to be separate.
def par[T, U](f: => T, g: => U): (T, U) = (f, g)

def main() =
  val c = Counters(0, 10)
  val c1 = c.iterator
  val c2 = c.iterator
  par(c1.exists(_ == 10), c2.exists(_ == 11))
  // par(c1.exists(_ == 10), c1.exists(_ == 11)) // this would fail
