object Test {
  def main(args: Array[String]): Unit = {
    val a = 1 + 2
    val b = a * 9
    val c = plus(a, b)
    println(c)
  }

  def plus(x: Int, y: Int) = {
    val a = x * x
    val b = y * y
    a + b
  }
}
