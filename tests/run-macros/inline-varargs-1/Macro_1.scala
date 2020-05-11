
import scala.quoted._

object Macros {
  def sum(using s: Scope)(nums: s.Expr[Int]*): s.Expr[Int] = Expr(nums.map(_.unliftOrError).sum)
}
