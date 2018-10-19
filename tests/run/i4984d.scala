object Array2 {
  def unapplySeq(x: Array[Int]): Data1 = new Data1

  class Data1 {
    def isEmpty: Boolean = false
    def get: Data2 = new Data2
  }

  class Data2 {
    def apply(i: Int): Int = 3
    def drop(n: Int): scala.Seq[Int] = Seq(2, 5)
    def toSeq: scala.Seq[Int] = Seq(6, 7)
    def lengthCompare(len: Int): Int = 0
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
