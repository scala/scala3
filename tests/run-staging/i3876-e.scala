import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

    def x given QuoteContext: Expr[Int] = '{ println(); 3 }

    def f4 given QuoteContext: Expr[Int => Int] = '{
      inlineLambda
    }
    println(run(f4(x)))
    println(withQuoteContext(f4(x).show))
  }

  inline def inlineLambda <: Int => Int = x => x + x
}