import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x(given QuoteContext): Expr[Int] = '{3}

    def f(given QuoteContext): Expr[Int => Int] = '{ (x: Int) => x + x }

    println(run(Expr.betaReduce(f)(x)))
    println(withQuoteContext(Expr.betaReduce(f)(x).show))
  }
}
