
import scala.quoted._

object Macros {
  def sum(nums: Int*): Expr[Int] = nums.sum.toExpr
}
