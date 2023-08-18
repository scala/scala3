//> using options -language:experimental.erasedDefinitions

object Test {
  erased class Erased() {
    println("Oh no!!!")
  }

  def f(x: Erased, y: Int = 0): Int = y + 5

  def g() = Erased()

  def main(args: Array[String]) =
    val y = Erased()
    val z = 10
    println(f(Erased()) + z + f(g(), 7))
}
