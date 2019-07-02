
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  def sum(nums: Int*) given QuoteContext: Expr[Int] = nums.sum
}
