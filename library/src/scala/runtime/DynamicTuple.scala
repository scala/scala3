package scala.runtime

import scala.Tuple.Concat
import scala.Tuple.Size
import scala.Tuple.Head
import scala.Tuple.Tail
import scala.Tuple.Elem

object DynamicTuple {
  inline val MaxSpecialized = 22
  inline private val XXL = MaxSpecialized + 1

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

  def dynamicFromProduct[T <: Tuple](xs: Product): T = (xs.productArity match {
    case 1 =>
      xs match {
        case xs: Tuple1[_] => xs
        case xs => Tuple1(xs.productElement(0))
      }
    case 2 =>
      xs match {
        case xs: Tuple2[_, _] => xs
        case xs => Tuple2(xs.productElement(0), xs.productElement(1))
      }
    case 3 =>
      xs match {
        case xs: Tuple3[_, _, _] => xs
        case xs => Tuple3(xs.productElement(0), xs.productElement(1), xs.productElement(2))
      }
    case 4 =>
      xs match {
        case xs: Tuple4[_, _, _, _] => xs
        case xs => Tuple4(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3))
      }
    case 5 =>
      xs match {
        case xs: Tuple5[_, _, _, _, _] => xs
        case xs => Tuple5(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4))
      }
    case 6 =>
      xs match {
        case xs: Tuple6[_, _, _, _, _, _] => xs
        case xs => Tuple6(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5))
      }
    case 7 =>
      xs match {
        case xs: Tuple7[_, _, _, _, _, _, _] => xs
        case xs => Tuple7(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6))
      }
    case 8 =>
      xs match {
        case xs: Tuple8[_, _, _, _, _, _, _, _] => xs
        case xs => Tuple8(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7))
      }
    case 9 =>
      xs match {
        case xs: Tuple9[_, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple9(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8))
      }
    case 10 =>
      xs match {
        case xs: Tuple10[_, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple10(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9))
      }
    case 11 =>
      xs match {
        case xs: Tuple11[_, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple11(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10))
      }
    case 12 =>
      xs match {
        case xs: Tuple12[_, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple12(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11))
      }
    case 13 =>
      xs match {
        case xs: Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple13(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12))
      }
    case 14 =>
      xs match {
        case xs: Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple14(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13))
      }
    case 15 =>
      xs match {
        case xs: Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple15(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14))
      }
    case 16 =>
      xs match {
        case xs: Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple16(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15))
      }
    case 17 =>
      xs match {
        case xs: Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple17(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16))
      }
    case 18 =>
      xs match {
        case xs: Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple18(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17))
      }
    case 19 =>
      xs match {
        case xs: Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple19(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18))
      }
    case 20 =>
      xs match {
        case xs: Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple20(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19))
      }
    case 21 =>
      xs match {
        case xs: Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple21(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20))
      }
    case 22 =>
      xs match {
        case xs: Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => xs
        case xs => Tuple22(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20), xs.productElement(21))
      }
    case _ =>
      xs match {
        case xs: TupleXXL => xs
        case xs => TupleXXL(xs.productIterator.map(_.asInstanceOf[Object]).toArray)
      }
  }).asInstanceOf[T]


  def dynamicToArray(self: Tuple): Array[Object] = (self: Any) match {
    case self: Unit => Array.emptyObjectArray
    case self: TupleXXL => self.toArray
    case self: Product => productToArray(self)
  }

  def productToArray(self: Product): Array[Object] = {
    val arr = new Array[Object](self.productArity)
    for (i <- 0 until arr.length) arr(i) = self.productElement(i).asInstanceOf[Object]
    arr
  }

  def dynamicCons[H, This <: Tuple](x: H, self: Tuple): H *: This = {
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

  def dynamicConcat[This <: Tuple, That <: Tuple](self: This, that: That): Concat[This, That] = {
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
    case self: Product => self.productArity.asInstanceOf[Size[This]]
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
      case self: Unit => throw new IndexOutOfBoundsException(n.toString)
      case self: Product => self.productElement(n)
    }
    res.asInstanceOf[Result]
  }

  def consIterator(head: Any, tail: Tuple): Iterator[Any] =
    Iterator.single(head) ++ tail.asInstanceOf[Product].productIterator

  def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any] =
    tup1.asInstanceOf[Product].productIterator ++ tup2.asInstanceOf[Product].productIterator
}
