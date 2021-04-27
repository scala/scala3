import scala.quoted.*
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {

    val sym = new Symantics {
      def Meth[R](exp: R): Int = 2
      def Meth(): Int = 42
    }

    val test = m(sym, 3)
  }
}
