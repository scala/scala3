
import scala.quoted._
import scala.quoted.autolift

object Macros {
  def sum(nums: Expr[Int]*) (using QuoteContext): Expr[Int] = nums.map(_.unliftOrError).sum
}
