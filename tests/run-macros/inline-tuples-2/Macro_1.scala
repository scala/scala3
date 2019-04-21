
import scala.quoted._
import scala.quoted.autolift._

object Macros {

  def impl(tup: Tuple1[Int]): Expr[Int] = tup._1

  def impl2(tup: Tuple1[Tuple1[Int]]): Expr[Int] = impl(tup._1)

}
