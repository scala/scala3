import scala.quoted.*
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {

    val sym = new Symantics[Int] {
      def Meth(exp: Int): Int = exp
      def Meth(): Int = 42
    }

    val test = m[Int](sym)
  }
}
