import Macros._

object Test {

  def main(args: Array[String]): Unit = {
    println(xyz"Hello World")
    println(xyz"Hello ${"World"}")
  }

}
