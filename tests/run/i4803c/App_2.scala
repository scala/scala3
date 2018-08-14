
object Test {
  def main(args: Array[String]): Unit = {
    class Num(x: Double) {
      transparent def power(n: Long & Constant) = ~PowerMacro.powerCode('(x), n)
    }
    val n = new Num(1.5)
    println(n.power(0L))
    println(n.power(1L))
    println(n.power(2L))
    println(n.power(5L))

    transparent def power(x: Double, n: Long & Constant) = ~PowerMacro.powerCode('(x), n)

    val x: Double = 1.5

    println(power(x, 0L))
    println(power(x, 1L))
    println(power(x, 2L))
    println(power(x, 5L))
  }
}
