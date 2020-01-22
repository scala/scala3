import scala.quoted._
import scala.quoted.autolift.{given _}

object Lib {
  inline def sum(inline args: Int*): Int = ${ impl('args) }
  def impl(args: Expr[Seq[Int]]) with QuoteContext : Expr[Int] = args.value.sum
}
