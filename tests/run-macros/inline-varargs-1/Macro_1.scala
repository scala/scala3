
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {
  def sum(nums: Expr[Int]*) (using QuoteContext): Expr[Int] = nums.map(_.value).sum
}
