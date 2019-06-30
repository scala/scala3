import scala.quoted._

object Test {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    withQuoteContext('[List])

    def list given QuoteContext = bound('{List(1, 2, 3)})
    println(withQuoteContext(list.show))
    println(run(list))

    def opt given QuoteContext = bound('{Option(4)})
    println(withQuoteContext(opt.show))
    println(run(opt))

    def map given QuoteContext = bound('{Map(4 -> 1)})
    println(withQuoteContext(map.show))
    println(run(map))
  }

  def bound[T: Type, S[_]: Type](x: Expr[S[T]]) given QuoteContext: Expr[S[T]] = '{
    val y: S[T] = $x
    y
  }
}
