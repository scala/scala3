object Array2 {
  def unapplySeq[T](x: Array[T]): UnapplySeqWrapper[T] = new UnapplySeqWrapper(x)

  final class UnapplySeqWrapper[T](private val a: Array[T]) extends AnyVal {
    def isEmpty: Boolean = false
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = a.lengthCompare(len)
    def apply(i: Int): T = a(i)
    def drop(n: Int): scala.Seq[T] = ???
    def toSeq: scala.Seq[T] = a.toSeq // clones the array
  }
}

class Test {
  def test1(xs: Array[Int]): Int = xs match {
    case Array2(x, y) => x + y
  }

  def test2(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(x, y, xs:_*) => xs
  }

  def test3(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(xs:_*) => xs
  }
}
