
import scala.quoted._

object Macros {
  def sum(nums: Expr[Int]*) (using Quotes): Expr[Int] = Value(nums.map(_.valueOrError).sum)
}
