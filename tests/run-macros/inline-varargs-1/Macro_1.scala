
import scala.quoted._
import scala.quoted.autolift.given

object Macros {
  def sum(nums: Int*) with QuoteContext : Expr[Int] = nums.sum
}
