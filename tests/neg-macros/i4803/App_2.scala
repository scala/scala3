
class Num2(x: Double) {
  inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
}

object Test {
  def main(args: Array[String]): Unit = {
    val n2 = new Num2(1.5)
    println(n2.power(0)) // error: Macro cannot be expanded in the same project as its definition
    println(n2.power(1)) // error: Macro cannot be expanded in the same project as its definition
    println(n2.power(2)) // error: Macro cannot be expanded in the same project as its definition
    println(n2.power(5)) // error: Macro cannot be expanded in the same project as its definition
  }
}
