
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {
  def sum(nums: Expr[Int]*) with QuoteContext : Expr[Int] = nums.map(_.value).sum
}
