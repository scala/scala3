
import scala.quoted._

object Macros {
  def sum(nums: Expr[Int]*) (using QuoteContext): Expr[Int] = Expr(nums.map(_.unliftOrError).sum)
}
