import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
    def expr given QuoteContext = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(run(expr))
    println(run(expr))
    println(withQuoteContext(expr.show))
  }
}
