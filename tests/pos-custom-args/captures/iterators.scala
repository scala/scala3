package cctest

abstract class Iterator[T]:
  thisIterator: {*} Iterator[T] =>

  def hasNext: Boolean
  def next: T
  def map(f: {*} T => T): {f, this} Iterator[T] = new Iterator:
    def hasNext = thisIterator.hasNext
    def next = f(thisIterator.next)
end Iterator

class C
type Cap = {*} C

def map[T, U](it: {*} Iterator[T], f: {*} T => U): {it, f} Iterator[U] = new Iterator:
  def hasNext = it.hasNext
  def next = f(it.next)

def test(c: Cap, d: Cap, e: Cap) =
  val it = new Iterator[Int]:
    private var ctr = 0
    def hasNext = ctr < 10
    def next = { ctr += 1; ctr }

  def f(x: Int): Int = if c == d then x else 10
  val it2 = it.map(f)
  val it3 = map(it, f)