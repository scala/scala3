
import dotty.tools.dotc.quoted.Runners._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val expr = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(expr.run)
    println(expr.run)
    println(expr.show)
  }
}
