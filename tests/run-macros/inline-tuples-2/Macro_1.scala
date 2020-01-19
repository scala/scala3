
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  def impl(tup: Tuple1[Int]) with QuoteContext : Expr[Int] = tup._1

  def impl2(tup: Tuple1[Tuple1[Int]]) with QuoteContext : Expr[Int] = impl(tup._1)

}
