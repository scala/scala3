import scala.quoted._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    def x = 2
    println(foo(1, 2, x))
  }
}
