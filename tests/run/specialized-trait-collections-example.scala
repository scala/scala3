//> using options -language:experimental.specializedTraits

inline trait Iterator[T: Specialized]:
  def hasNext: Boolean
  def next(): T

inline trait ArrayIterator[T: Specialized](elems: Array[T]) extends Iterator[T]:
  private var current = 0
  def hasNext: Boolean = current < elems.length
  def next(): T = try elems(current) finally current += 1

inline trait Iterable[T: Specialized]:
  def iterator: Iterator[T]
  def forall(f: T => Unit): Unit =
    val it = iterator
    while it.hasNext do f(it.next())

inline trait Seq[T: Specialized](elems: Array[T]) extends Iterable[T]:
  def length: Int           = elems.length
  def apply(i: Int): T      = elems(i)
  def iterator: Iterator[T] = new ArrayIterator[T](elems) {}

@main def Test =
  val elems = Array.from(scala.collection.immutable.Seq(1, 2, 3, 4, 5))
  val seq = new Seq[Int](elems) {}
  var x = 0
  seq.forall(v => x += v)
  assert(x == 15)
