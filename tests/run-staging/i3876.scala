import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

    def x given QuoteContext: Expr[Int] = '{3}

    def f given QuoteContext: Expr[Int => Int] = '{ (x: Int) => x + x }

    println(run(f(x)))
    println(withQuoteContext(f(x).show))
  }
}
