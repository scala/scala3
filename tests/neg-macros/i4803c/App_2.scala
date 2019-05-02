
object Test {
  def main(args: Array[String]): Unit = {
    class Num(x: Double) {
      inline def power(inline n: Long) = ${ PowerMacro.powerCode('x, n) }
    }
    val n = new Num(1.5)
    println(n.power(0)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(1)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(2)) // error: Macro cannot be expanded in the same project as its definition
    println(n.power(5)) // error: Macro cannot be expanded in the same project as its definition

    inline def power(x: Double, inline n: Long) = ${ PowerMacro.powerCode('x, n) }

    val x: Double = 1.5

    println(power(x, 0)) // error: Macro cannot be expanded in the same project as its definition
    println(power(x, 1)) // error: Macro cannot be expanded in the same project as its definition
    println(power(x, 2)) // error: Macro cannot be expanded in the same project as its definition
    println(power(x, 5)) // error: Macro cannot be expanded in the same project as its definition
  }
}
