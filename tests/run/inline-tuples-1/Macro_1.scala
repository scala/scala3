
import scala.quoted._

object Macros {
  def tup1(tup: Tuple1[Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup2(tup: Tuple2[Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup3(tup: Tuple3[Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup4(tup: Tuple4[Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup5(tup: Tuple5[Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup6(tup: Tuple6[Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup7(tup: Tuple7[Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup8(tup: Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup9(tup: Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup10(tup: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup11(tup: Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup12(tup: Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup13(tup: Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup14(tup: Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup15(tup: Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup16(tup: Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup17(tup: Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup18(tup: Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup19(tup: Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup20(tup: Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup21(tup: Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
  def tup22(tup: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Expr[Int] = tup.productIterator.map(_.asInstanceOf[Int]).sum.toExpr
}
