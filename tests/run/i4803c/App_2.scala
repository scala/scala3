
object Test {
  def main(args: Array[String]): Unit = {
    class Num(x: Double) {
      inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
    }
    val n = new Num(1.5)
    println(n.power(0))
    println(n.power(1))
    println(n.power(2))
    println(n.power(5))

    inline def power(x: Double, inline n: Long) = ${ PowerMacro.powerCode('x, n) }

    val x: Double = 1.5

    println(power(x, 0))
    println(power(x, 1))
    println(power(x, 2))
    println(power(x, 5))
  }
}
