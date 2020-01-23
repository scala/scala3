import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x with QuoteContext : Expr[Int] = '{3}

    def f2 with QuoteContext : Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }

    println(run(Expr.betaReduce(f2)(x)))
    println(withQuoteContext(Expr.betaReduce(f2)(x).show))
  }
}
