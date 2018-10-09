package scala
import annotation.showAsInfix
import typelevel._

sealed trait Tuple extends Any {
  import Tuple._
  import StagedTuple._

  inline def toArray: Array[Object] =
    ~toArrayStaged('(this), constValueOpt[BoundedSize[this.type]])

  inline def *: [H] (x: H): H *: this.type =
    ~stagedCons('(this), '(x), constValueOpt[BoundedSize[this.type]])

  inline def ++(that: Tuple): Concat[this.type, that.type] =
    ~stagedConcat('(this), constValueOpt[BoundedSize[this.type]], '(that), constValueOpt[BoundedSize[that.type]])

  inline def size: Size[this.type] =
    ~sizeStaged[Size[this.type]]('(this), constValueOpt[BoundedSize[this.type]])

}

object Tuple {
  import StagedTuple._

  inline val $MaxSpecialized = 22
  inline private val XXL = $MaxSpecialized + 1

  final val specialize = false

  type Head[+X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  type Tail[+X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  type Concat[+X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[+X <: Tuple, +N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Size[+X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  private[scala] type BoundedSizeRecur[X, L <: Int] <: Int = X match {
    case Unit => 0
    case x *: xs =>
      L match {
        case 0 => 0
        case S[n] => S[BoundedSizeRecur[xs, n]]
      }
  }

  private[scala] type BoundedSize[X] = BoundedSizeRecur[X, 23]

  val $emptyArray = Array[Object]()

  def $toArray(xs: Tuple, n: Int) = {
    val arr = new Array[Object](n)
    var i = 0
    var it = xs.asInstanceOf[Product].productIterator
    while (i < n) {
      arr(i) = it.next().asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def $consArray[H](x: H, elems: Array[Object]): Array[Object] = {
    val elems1 = new Array[Object](elems.length + 1)
    elems1(0) = x.asInstanceOf[Object]
    Array.copy(elems, 0, elems1, 1, elems.length)
    elems1
  }

  inline def fromArray[T <: Tuple](xs: Array[Object]): T =
    ~fromArrayStaged[T]('(xs), constValueOpt[BoundedSize[this.type]])

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
      $emptyArray
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
      asInstanceOf[TupleXXL].elems
    case self: Product =>
      val arr = new Array[Object](self.productArity)
      for (i <- 0 until arr.length) arr(i) = self.productElement(i).asInstanceOf[Object]
      arr
  }

  def dynamic_*: [This <: Tuple, H] (self: Tuple, x: H): H *: This = {
    type Result = H *: This
    (self: Any) match {
      case Unit =>
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
        dynamicFromArray[Result]($consArray(x, dynamicToArray(self)))
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
}

abstract sealed class NonEmptyTuple extends Tuple {
  import Tuple._
  import NonEmptyTuple._
  import StagedTuple._

  inline def head: Head[this.type] =
    ~headStaged[this.type]('(this), constValueOpt[BoundedSize[this.type]])

  inline def tail: Tail[this.type] =
    ~tailStaged[this.type]('(this), constValueOpt[BoundedSize[this.type]])

  inline def apply(n: Int): Elem[this.type, n.type] =
    ~applyStaged[this.type, n.type]('(this), constValueOpt[Size[this.type]], '(n), constValueOpt[n.type])

}

object NonEmptyTuple {
  import Tuple._

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

  def dynamicApply[This <: NonEmptyTuple] (self: This, n: Int): Elem[This, n.type] = {
    type Result = Elem[This, n.type]
    val res = (self: Any) match {
      case self: TupleXXL => self.elems(n)
      case self: Product => self.productElement(n)
    }
    res.asInstanceOf[Result]
  }
}

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  def unapply[H, T <: Tuple](x: H *: T): (H, T) =
    (NonEmptyTuple.dynamicHead(x), NonEmptyTuple.dynamicTail(x))
}
