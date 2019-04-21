
object Test {
  def main(args: Array[String]): Unit = {
    val x1: Double = 0
    val x2: Double = 1.5
    val x3: Double = 3.5

    println(power2(x1))
    println(power2(x2))
    println(power2(x3))
  }

  inline def power2(x: Double) = ${PowerMacro.power2('x)}
}
