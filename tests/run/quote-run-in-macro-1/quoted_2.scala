import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    println(foo(1))
    println(foo(1  + 3))
    val x = 3
    println(foo {
      val x = 5
      x
    })
  }
}
