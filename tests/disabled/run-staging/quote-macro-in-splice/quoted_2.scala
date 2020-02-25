import scala.quoted._
import scala.quoted.staging._
import Macros._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val x = '{
      val y = 1
      ${
        inline def a(z: Int): Int = ${ impl('z) }
        val b = Expr(a(7))
        '{ y + $b }
      }
    }
    println(x.show)
  }
}
