import scala.quoted._
import scala.quoted.staging._
import Macros._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val x = '{
      val y = 1
      ${
        inline def a(z: Int): Int = ${ impl('z) }
        val b = Value(a(7))
        '{ y + $b }
      }
    }
    println(x.show)
  }
}
