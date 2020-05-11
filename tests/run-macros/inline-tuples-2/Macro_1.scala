
import scala.quoted._

object Macros {

  def impl(using s: Scope)(tup: s.Expr[Tuple1[Int]]): s.Expr[Int] = Expr(tup.unliftOrError._1)

  def impl2(using s: Scope)(tup: s.Expr[Tuple1[Tuple1[Int]]]): s.Expr[Int] = impl(Expr(tup.unliftOrError._1))

}
