import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    println(foo(x => "> " + x))
    println(foo(x => "> " + 4 * x))
    val y = 9
    println(foo(x => "> " + y))
  }
}
