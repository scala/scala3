import quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
    def q given QuoteContext = f
    println(run(q))
    println(withQuoteContext(q.show))
  }

  def f given QuoteContext: Expr[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g given QuoteContext: Expr[Int] = '{
    val a = 9
    a + 0
  }
}
