

class Nums {
  class Num(x: Double) {
    inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val nums = new Nums
    val n = new nums.Num(1.5)
    println(n.power(0)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(1)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(2)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(5)) // error: Macro cannot be expanded in the same project as its definition
  }
}
