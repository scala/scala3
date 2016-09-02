object Test {

  @dotty.annotation.inline
  def power(x: Double, n: Int): Double =
    if (n == 0) 1.0
    else if (n == 1) x
    else {
      val y = power(x, n / 2)
      if (n % 2 == 0) y * y else y * y * x
    }

  def main(args: Array[String]): Unit = {
    println(power(2.0, 10))
    def x = 2.0
    println(power(x, 11))
  }
}
