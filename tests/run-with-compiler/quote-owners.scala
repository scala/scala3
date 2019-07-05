import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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
