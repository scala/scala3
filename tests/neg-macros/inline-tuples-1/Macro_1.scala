
import scala.quoted.*

object Macros {
  def tup1(tup: Expr[Tuple1[Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup2(tup: Expr[Tuple2[Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup3(tup: Expr[Tuple3[Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup4(tup: Expr[Tuple4[Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup5(tup: Expr[Tuple5[Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup6(tup: Expr[Tuple6[Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup7(tup: Expr[Tuple7[Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup8(tup: Expr[Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup9(tup: Expr[Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup10(tup: Expr[Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup11(tup: Expr[Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup12(tup: Expr[Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup13(tup: Expr[Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup14(tup: Expr[Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup15(tup: Expr[Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup16(tup: Expr[Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup17(tup: Expr[Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup18(tup: Expr[Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup19(tup: Expr[Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup20(tup: Expr[Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup21(tup: Expr[Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup22(tup: Expr[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]) (using Quotes): Expr[Int] = Expr(tup.valueOrError.productIterator.map(_.asInstanceOf[Int]).sum)
}
