
import scala.quoted.*

object Macros {
  def sum(nums: Expr[Int]*) (using Quotes): Expr[Int] = Expr(nums.map(_.valueOrError).sum)
}
