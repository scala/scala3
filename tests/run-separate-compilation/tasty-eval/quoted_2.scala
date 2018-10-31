
import Macros._

object Test {
  final val y = 5

  def main(args: Array[String]): Unit = {
    println(foo(1)) // "Some(1)"
    println(foo(1 + 7)) // "Some(8)"
    println(foo(y)) // "Some(5)"
    println(foo(y + 1))
    val x = 4
    println(foo(x)) // "None"
  }
}
