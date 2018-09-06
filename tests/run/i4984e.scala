object Array2 {
  def unapplySeq(x: Array[Int]): Data = new Data

  final class Data {
    def isEmpty: Boolean = false
    def get: Data = this
    def lengthCompare(len: Int): Int = 0
    def lengthCompare: Int = 0
    def apply(i: Int): Int = 3
    def apply(i: String): Int = 3
    def drop(n: Int): scala.Seq[Int] = Seq(2, 5)
    def drop: scala.Seq[Int] = Seq(2, 5)
    def toSeq: scala.Seq[Int] = Seq(6, 7)
    def toSeq(x: Int): scala.Seq[Int] = Seq(6, 7)
  }
}

object Test {
  def test1(xs: Array[Int]): Int = xs match {
    case Array2(x, y) => x + y
  }

  def test2(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(x, y, xs:_*) => xs
  }

  def test3(xs: Array[Int]): Seq[Int] = xs match {
    case Array2(xs:_*) => xs
  }

  def main(args: Array[String]): Unit = {
    test1(Array(3, 5))
    test2(Array(3, 5))
    test3(Array(3, 5))
  }
}
