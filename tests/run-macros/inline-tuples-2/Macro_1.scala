
import scala.quoted._
import scala.quoted.autolift

object Macros {

  def impl(tup: Expr[Tuple1[Int]]) (using QuoteContext): Expr[Int] = tup.unliftOrError._1

  def impl2(tup: Expr[Tuple1[Tuple1[Int]]]) (using QuoteContext): Expr[Int] = impl(tup.unliftOrError._1)

}
