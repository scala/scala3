import scala.quoted._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    println("Normal function")
    println(foreach1(0, 5, x => println(x)))
    println()

    println("By name function")
    println(foreach2(0, 5, x => println(x)))
  }
}
