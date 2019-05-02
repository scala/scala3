
import scala.quoted._
import scala.quoted.autolift._

object Macros {
  inline def sum(inline tup: Tuple1[Int]): Int = ${ Macros.tup1(tup) }
  inline def sum(inline tup: Tuple2[Int, Int]): Int = ${ Macros.tup2(tup) }
  inline def sum(inline tup: Tuple3[Int, Int, Int]): Int = ${ Macros.tup3(tup) }
  inline def sum(inline tup: Tuple4[Int, Int, Int, Int]): Int = ${ Macros.tup4(tup) }
  inline def sum(inline tup: Tuple5[Int, Int, Int, Int, Int]): Int = ${ Macros.tup5(tup) }
  inline def sum(inline tup: Tuple6[Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup6(tup) }
  inline def sum(inline tup: Tuple7[Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup7(tup) }
  inline def sum(inline tup: Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup8(tup) }
  inline def sum(inline tup: Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup9(tup) }
  inline def sum(inline tup: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup10(tup) }
  inline def sum(inline tup: Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup11(tup) }
  inline def sum(inline tup: Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup12(tup) }
  inline def sum(inline tup: Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup13(tup) }
  inline def sum(inline tup: Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup14(tup) }
  inline def sum(inline tup: Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup15(tup) }
  inline def sum(inline tup: Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup16(tup) }
  inline def sum(inline tup: Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup17(tup) }
  inline def sum(inline tup: Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup18(tup) }
  inline def sum(inline tup: Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup19(tup) }
  inline def sum(inline tup: Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup20(tup) }
  inline def sum(inline tup: Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup21(tup) }
  inline def sum(inline tup: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup22(tup) }

  def tup1(tup: Tuple1[Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup2(tup: Tuple2[Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup3(tup: Tuple3[Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup4(tup: Tuple4[Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup5(tup: Tuple5[Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup6(tup: Tuple6[Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup7(tup: Tuple7[Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup8(tup: Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup9(tup: Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup10(tup: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup11(tup: Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup12(tup: Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup13(tup: Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup14(tup: Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup15(tup: Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup16(tup: Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup17(tup: Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup18(tup: Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup19(tup: Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup20(tup: Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup21(tup: Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
  def tup22(tup: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum
}
