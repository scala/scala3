import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    println(foo(1)) // error
    println(foo(1  + 3)) // error
    val x = 3
    println(foo { // error
      val x = 5
      x
    })
  }
}
