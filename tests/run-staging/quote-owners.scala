import quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def q with QuoteContext = f
    println(run(q))
    println(withQuoteContext(q.show))
  }

  def f with QuoteContext : Expr[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g with QuoteContext : Expr[Int] = '{
    val a = 9
    a + 0
  }
}
