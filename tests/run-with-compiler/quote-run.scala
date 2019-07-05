import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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
