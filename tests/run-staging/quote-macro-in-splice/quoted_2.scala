import scala.quoted._
import scala.quoted.staging._
import Macros._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val x = '{
      val y = 1
      ${
        // FIXME remove context when $ will provide one
        // Currently we would accidentally capture the one from withQuoteContext
        inline def a(z: Int): Int = ${ impl('z) given QuoteContext.macroContext }
        val b = a(7).toExpr
        '{ y + $b }
      }
    }
    println(x.show)
  }

}
