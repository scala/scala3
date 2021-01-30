object Test {
  import Macro.*

  def main(args: Array[String]): Unit = {
    println(openTest((x: Int) => x))
    println(openTest((x: Int) => x * x))
    println()
    println(openTest((x1: Int, x2: Int) => x1 + x2))
    println(openTest((x1: Int, x2: Int) => x1 * x2))
    println()
    println(openTest((x1: Int, x2: Int, x3: Int) => x1 + x2 + x3))
    println(openTest((x1: Int, x2: Int, x3: Int) => x1 * x2 * x3))
  }

}
