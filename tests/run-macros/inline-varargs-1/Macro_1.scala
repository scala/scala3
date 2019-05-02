
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  inline def sum(inline i: Int, inline j: Int, inline k: Int): Int = ${ Macros.sum(i, j, k) }
  def sum(nums: Int*): Expr[Int] = nums.sum
}
