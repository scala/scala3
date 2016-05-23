trait Comparator {
    type T     // abstract type member, to be filled in by concrete classes
    def ordering: Ordering[T]
    def compare(a: T, b: T): Int = ordering.compare(a, b)
}

object IntComparator extends Comparator {
    type T = Int
    def ordering: Ordering[Int] = Ordering.Int
}
object Test {
  def process(c: Comparator)(items: Seq[c.T]): Int = {
    c.compare(items(0), items(1))
  }
}
class Processor[K](c: Comparator { type T = K }) {
  def process(items: Seq[K]): Int = {
    c.compare(items(0), items(1))
  }
}
