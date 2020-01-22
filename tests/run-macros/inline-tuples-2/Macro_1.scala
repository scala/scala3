
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  def impl(tup: Expr[Tuple1[Int]]) with QuoteContext : Expr[Int] = tup.value._1

  def impl2(tup: Expr[Tuple1[Tuple1[Int]]]) with QuoteContext : Expr[Int] = impl(tup.value._1)

}
