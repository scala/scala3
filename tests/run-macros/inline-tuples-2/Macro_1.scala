
import scala.quoted._
import scala.quoted.autolift._

object Macros {

  def impl(tup: Tuple1[Int]): Expr[Int] = tup._1

  def impl2(tup: Tuple1[Tuple1[Int]]): Expr[Int] = impl(tup._1)

  inline def get1(inline tup: Tuple1[Int]): Int = ${ Macros.impl(tup) }

  inline def get2(inline i: Int): Int = ${ Macros.impl(Tuple1(i)) }

  inline def get3(inline i: Int): Int = ${ Macros.impl2(Tuple1(Tuple1(i))) }

  inline def get4(inline tup: Tuple1[Tuple1[Int]]): Int = ${ Macros.impl2(tup) }
}
