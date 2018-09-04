object Array2 {
  def unapplySeq(x: Array[Int]): Data = new Data

  final class Data {
    def isEmpty: Boolean = false
    def get: Data = this
    def lengthCompare(len: Int): Int = 0
    def apply(i: Int): Int = 3
    def drop(n: Int): scala.Seq[String] = Seq("hello")
    def toSeq: scala.Seq[Int] = Seq(6, 7)
  }
}

object Test {
  def test1(xs: Array[Int]): Int = xs match {
    case Array2(x, y) => x + y  // error // error
  }

  def test2(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(x, y, xs:_*) => xs // error
  }

  def test3(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(xs:_*) => xs   // error
  }
}
