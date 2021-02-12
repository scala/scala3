import Macros.*
object Test {
  def main(args: Array[String]): Unit = {
    val x = 3
    println(foo(x)) // error
  }
}
