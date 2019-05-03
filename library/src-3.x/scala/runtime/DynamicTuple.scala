package scala.runtime

import scala.Tuple.Concat
import scala.Tuple.Size
import scala.Tuple.Head
import scala.Tuple.Tail
import scala.Tuple.Elem

object DynamicTuple {
  inline val MaxSpecialized = 22
  inline private val XXL = MaxSpecialized + 1

  val empty$Array = Array[Object]()

  def to$Array(xs: Tuple, n: Int) = {
    val arr = new Array[Object](n)
    var i = 0
    var it = xs.asInstanceOf[Product].productIterator
    while (i < n) {
      arr(i) = it.next().asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def cons$Array[H](x: H, elems: Array[Object]): Array[Object] = {
    val elems1 = new Array[Object](elems.length + 1)
    elems1(0) = x.asInstanceOf[Object]
    System.arraycopy(elems, 0, elems1, 1, elems.length)
    elems1
  }

  def dynamicFromArray[T <: Tuple](xs: Array[Object]): T = xs.length match {
    case 0  => ().asInstanceOf[T]
    case 1  => Tuple1(xs(0)).asInstanceOf[T]
    case 2  => Tuple2(xs(0), xs(1)).asInstanceOf[T]
    case 3  => Tuple3(xs(0), xs(1), xs(2)).asInstanceOf[T]
    case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3)).asInstanceOf[T]
    case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4)).asInstanceOf[T]
    case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5)).asInstanceOf[T]
    case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6)).asInstanceOf[T]
    case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7)).asInstanceOf[T]
    case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8)).asInstanceOf[T]
    case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9)).asInstanceOf[T]
    case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10)).asInstanceOf[T]
    case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11)).asInstanceOf[T]
    case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12)).asInstanceOf[T]
    case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13)).asInstanceOf[T]
    case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14)).asInstanceOf[T]
    case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15)).asInstanceOf[T]
    case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16)).asInstanceOf[T]
    case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17)).asInstanceOf[T]
    case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18)).asInstanceOf[T]
    case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19)).asInstanceOf[T]
    case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20)).asInstanceOf[T]
    case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21)).asInstanceOf[T]
    case _ => TupleXXL(xs).asInstanceOf[T]
  }

  def dynamicToArray(self: Tuple): Array[Object] = (self: Any) match {
    case self: Unit =>
      scala.runtime.DynamicTuple.empty$Array
    case self: Tuple1[_] =>
      val t = self.asInstanceOf[Tuple1[Object]]
      Array(t._1)
    case self: Tuple2[_, _] =>
      val t = self.asInstanceOf[Tuple2[Object, Object]]
      Array(t._1, t._2)
    case self: Tuple3[_, _, _] =>
      val t = self.asInstanceOf[Tuple3[Object, Object, Object]]
      Array(t._1, t._2, t._3)
    case self: Tuple4[_, _, _, _] =>
      val t = self.asInstanceOf[Tuple4[Object, Object, Object, Object]]
      Array(t._1, t._2, t._3, t._4)
    case self: TupleXXL =>
      self.elems
    case self: Product =>
      val arr = new Array[Object](self.productArity)
      for (i <- 0 until arr.length) arr(i) = self.productElement(i).asInstanceOf[Object]
      arr
  }

  def dynamic_*: [This <: Tuple, H] (self: Tuple, x: H): H *: This = {
    type Result = H *: This
    (self: Any) match {
      case () =>
        Tuple1(x).asInstanceOf[Result]
      case self: Tuple1[_] =>
        Tuple2(x, self._1).asInstanceOf[Result]
      case self: Tuple2[_, _] =>
        Tuple3(x, self._1, self._2).asInstanceOf[Result]
      case self: Tuple3[_, _, _] =>
        Tuple4(x, self._1, self._2, self._3).asInstanceOf[Result]
      case self: Tuple4[_, _, _, _] =>
        Tuple5(x, self._1, self._2, self._3, self._4).asInstanceOf[Result]
      case _ =>
        dynamicFromArray[Result](cons$Array(x, dynamicToArray(self)))
    }
  }

  def dynamic_++[This <: Tuple, That <: Tuple](self: This, that: That): Concat[This, That] = {
    type Result = Concat[This, That]
    (this: Any) match {
      case self: Unit => return self.asInstanceOf[Result]
      case _ =>
    }
    (that: Any) match {
      case that: Unit => return self.asInstanceOf[Result]
      case _ =>
    }
    dynamicFromArray[Result](dynamicToArray(self) ++ dynamicToArray(that))
  }

  def dynamicSize[This <: Tuple](self: This): Size[This] = (self: Any) match {
    case self: Unit => 0.asInstanceOf[Size[This]]
    case self: TupleXXL => self.elems.length.asInstanceOf[Size[This]]
    case self: Product => self.productArity.asInstanceOf[Size[This]]
  }

  def dynamicHead[This <: NonEmptyTuple] (self: This): Head[This] = {
    type Result = Head[This]
    val res = (self: Any) match {
      case self: Tuple1[_] => self._1
      case self: Tuple2[_, _] => self._1
      case self: Tuple3[_, _, _] => self._1
      case self: Tuple4[_, _, _, _] => self._1
      case self: TupleXXL => self.elems(0)
      case self: Product => self.productElement(0)
    }
    res.asInstanceOf[Result]
  }

  def dynamicTail[This <: NonEmptyTuple] (self: This): Tail[This] = {
    type Result = Tail[This]
    val res = (self: Any) match {
      case self: Tuple1[_] => ()
      case self: Tuple2[_, _] => Tuple1(self._2)
      case self: Tuple3[_, _, _] => Tuple2(self._2, self._3)
      case self: Tuple4[_, _, _, _] => Tuple3(self._2, self._3, self._4)
      case _ => dynamicFromArray[Result](dynamicToArray(self).tail)
    }
    res.asInstanceOf[Result]
  }

  def dynamicApply[This <: NonEmptyTuple, N <: Int] (self: This, n: Int): Elem[This, N] = {
    type Result = Elem[This, N]
    val res = (self: Any) match {
      case self: TupleXXL => self.elems(n)
      case self: Product => self.productElement(n)
    }
    res.asInstanceOf[Result]
  }
}
