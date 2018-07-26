
import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

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
