
import scala.quoted._

object Macros {
  def tup1(using s: Scope)(tup: s.Expr[Tuple1[Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup2(using s: Scope)(tup: s.Expr[Tuple2[Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup3(using s: Scope)(tup: s.Expr[Tuple3[Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup4(using s: Scope)(tup: s.Expr[Tuple4[Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup5(using s: Scope)(tup: s.Expr[Tuple5[Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup6(using s: Scope)(tup: s.Expr[Tuple6[Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup7(using s: Scope)(tup: s.Expr[Tuple7[Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup8(using s: Scope)(tup: s.Expr[Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup9(using s: Scope)(tup: s.Expr[Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup10(using s: Scope)(tup: s.Expr[Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup11(using s: Scope)(tup: s.Expr[Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup12(using s: Scope)(tup: s.Expr[Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup13(using s: Scope)(tup: s.Expr[Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup14(using s: Scope)(tup: s.Expr[Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup15(using s: Scope)(tup: s.Expr[Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup16(using s: Scope)(tup: s.Expr[Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup17(using s: Scope)(tup: s.Expr[Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup18(using s: Scope)(tup: s.Expr[Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup19(using s: Scope)(tup: s.Expr[Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup20(using s: Scope)(tup: s.Expr[Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup21(using s: Scope)(tup: s.Expr[Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
  def tup22(using s: Scope)(tup: s.Expr[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]): s.Expr[Int] = Expr(tup.unliftOrError.productIterator.map(_.asInstanceOf[Int]).sum)
}
