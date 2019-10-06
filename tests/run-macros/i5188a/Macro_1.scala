import scala.quoted._
import scala.quoted.autolift.given

object Lib {
  inline def sum(inline args: Int*): Int = ${ impl(args: _*) }
  def impl(args: Int*)(given QuoteContext): Expr[Int] = args.sum
}
