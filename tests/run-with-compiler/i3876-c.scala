import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit def toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    def x given QuoteContext: Expr[Int] = '{3}

    def f3 given QuoteContext: Expr[Int => Int] = '{
      val f: (x: Int) => Int = x => x + x
      f
    }

    println(run(f3(x)))
    println(withQuoteContext(f3(x).show)) // TODO improve printer
  }
}
