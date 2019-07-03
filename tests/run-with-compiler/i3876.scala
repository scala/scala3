import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    def x given QuoteContext: Expr[Int] = '{3}

    def f given QuoteContext: Expr[Int => Int] = '{ (x: Int) => x + x }

    println(run(f(x)))
    println(withQuoteContext(f(x).show))
  }
}
