import scala.quoted._
import Macros._

import scala.quoted.Toolbox.Default._

object Test {
  def main(args: Array[String]): Unit = {
    val x = '{
      val y = 1
      ${
        inline def a(z: Int): Int = ${ impl('z) }
        val b = a(7).toExpr
        '{ y + $b }
      }
    }
    println(x.show)
  }

}
