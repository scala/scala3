import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

    def x given QuoteContext: Expr[Int] = '{3}

    def f2 given QuoteContext: Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }

    println(run(f2(x)))
    println(withQuoteContext(f2(x).show))
  }
}
