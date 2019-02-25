

class Nums {
  class Num(x: Double) {
    inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val nums = new Nums
    val n = new nums.Num(1.5)
    println(n.power(0))
    println(n.power(1))
    println(n.power(2))
    println(n.power(5))
  }
}
