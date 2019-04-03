import scala.quoted._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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
