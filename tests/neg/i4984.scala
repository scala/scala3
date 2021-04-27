object Array2 {
  def unapplySeq(x: Array[Int]): Data = new Data
  class Data {
    def isEmpty: Boolean = false
    def get: Data = this
    def lengthCompare(len: Int): Int = 0
    def apply(i: Int): Int = 3
    // drop return type, not conforming to apply's
    def drop(n: Int): scala.Seq[String] = Seq("hello")
    def toSeq: scala.Seq[Int] = Seq(6, 7)
  }
}

object Array3 {
  def unapplySeq(x: Array[Int]): Data = new Data
  class Data {
    def isEmpty: Boolean = false
    def get: Data = this
    def lengthCompare(len: Int): Int = 0
    // missing apply
    def drop(n: Int): scala.Seq[Int] = ???
    def toSeq: scala.Seq[Int] = ???
  }
}

object Test {
  def test(xs: Array[Int]): Int = xs match {
    case Array2(x, y)         => 1 // error
    case Array2(x, y, xs*) => 2 // error
    case Array2(xs*)       => 3 // error
  }

  def test2(xs: Array[Int]): Int = xs match {
    case Array3(x, y)         => 1 // error
    case Array3(x, y, xs*) => 2 // error
    case Array3(xs*)       => 3 // error
  }
}
