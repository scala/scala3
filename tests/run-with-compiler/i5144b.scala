import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]) given QuoteContext: Expr[Int] = ff('{42})

  def peval1() given QuoteContext: Expr[Unit] = '{
    def f(x: Int): Int = ${eval1('f)}
  }

  def main(args: Array[String]): Unit = withQuoteContext {
    val p = peval1()
    println(p.show)
  }

}