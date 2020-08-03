import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    withQuoteContext('[List])

    def list(using QuoteContext) = bound('{List(1, 2, 3)})
    println(withQuoteContext(list.show))
    println(run(list))

    def opt(using QuoteContext) = bound('{Option(4)})
    println(withQuoteContext(opt.show))
    println(run(opt))

    def map(using QuoteContext) = bound('{Map(4 -> 1)})
    println(withQuoteContext(map.show))
    println(run(map))
  }

  def bound[T: Staged, S[_]: Staged](x: Expr[S[T]])(using QuoteContext): Expr[S[T]] = '{
    val y: S[T] = $x
    y
  }
}
