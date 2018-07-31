object Test {

  class Range(from: Int, end: Int) {

    transparent
    def foreach(op: => Int => Unit): Unit = {
      var i = from
      while (i < end) {
        op(i)
        i += 1
      }
    }

    def filter(p: Int => Boolean): List[Int] = ???
  }

  implicit class intWrapper(private val start: Int) extends AnyVal {
    def until(end: Int) = new Range(start, end)
    def to(limit: Int) = new Range(start, limit + 1)
  }

  def matmul(xs: Array[Array[Double]], ys: Array[Array[Double]]): Array[Array[Double]] = {
    def nrows = xs.length
    def ncols = ys(0).length
    def n = ys.length
    assert(xs(0).length == n)
    val zs = Array.ofDim[Double](nrows, ncols)
    for (i <- intWrapper(0) until nrows)
      for (j <- 0 until ncols) {
        var x = 0.0
        for (k <- 0 until n)
          x += xs(i)(k) * ys(k)(j)
        zs(i)(j) = x
      }
    zs
  }

  implicit class intArrayOps(arr: Array[Int]) {
    transparent def foreach(op: => Int => Unit): Unit = {
      var i = 0
      while (i < arr.length) {
        op(arr(i))
        i += 1
      }
    }
  }

  def sum(ints: Array[Int]): Int = {
    var t = 0
    for (n <- ints) t += n
    t
  }

  def main(args: Array[String]) = {
    1.until(10).foreach(i => println(i))
    1.until(10).foreach(println(_))
    1.until(10).foreach(println)
    for (i <- 1 to 10) println(i)

    for (k1 <- 1 to 10)
      for (k2 <- 1 to 10)
        println(s"$k1")

    val xs = Array(1, 2, 3, 4)
    assert(sum(xs) == 10, sum(xs))
  }
}
