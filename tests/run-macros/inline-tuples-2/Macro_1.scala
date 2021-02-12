
import scala.quoted.*

object Macros {

  def impl(tup: Expr[Tuple1[Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError._1)

  def impl2(tup: Expr[Tuple1[Tuple1[Int]]]) (using Quotes): Expr[Int] = impl(Expr(tup.valueOrError._1))

}
