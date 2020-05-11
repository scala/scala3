object Test {

  def main(args: Array[String]): Unit = {
    val a: Int = 5
    val b: Int = 6
    rewrite(plus(1, 4))
    rewrite(plus(0, a))
    rewrite(plus(a, b))
    rewrite(plus(plus(a, 0), plus(0, b)))
    rewrite(power(4, 5))
    rewrite(power(a, 5))
  }

}
