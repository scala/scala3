

class Nums {
  class Num(x: Double) {
    transparent def power(n: Long & Constant) = ~PowerMacro.powerCode('(x), n)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val nums = new Nums
    val n = new nums.Num(1.5)
    println(n.power(0L))
    println(n.power(1L))
    println(n.power(2L))
    println(n.power(5L))
  }
}
