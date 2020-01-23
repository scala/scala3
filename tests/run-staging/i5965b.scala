import scala.quoted._
import scala.quoted.staging._

object Test {

  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    withQuoteContext('[List])
    def list with QuoteContext = bound('{List(1, 2, 3)})
    println(withQuoteContext(list.show))
    println(run(list))

    def opt with QuoteContext = bound('{Option(4)})
    println(withQuoteContext(opt.show))
    println(run(opt))

    def map with QuoteContext = bound('{Map(4 -> 1)})
    println(withQuoteContext(map.show))
    println(run(map))
  }

  def bound[T: Type, S[_]: Type](x: Expr[S[T]]) with QuoteContext : Expr[S[T]] = '{
    val y = $x
    y
  }
}
