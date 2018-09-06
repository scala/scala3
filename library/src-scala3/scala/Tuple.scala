package scala
import annotation.showAsInfix
import typelevel._

sealed trait Tuple extends Any {
  import Tuple._

  rewrite def toArray: Array[Object] = rewrite constValueOpt[BoundedSize[this.type]] match {
    case Some(0) =>
      $emptyArray
    case Some(1) =>
      val t = asInstanceOf[Tuple1[Object]]
      Array(t._1)
    case Some(2) =>
      val t = asInstanceOf[Tuple2[Object, Object]]
      Array(t._1, t._2)
    case Some(3) =>
      val t = asInstanceOf[Tuple3[Object, Object, Object]]
      Array(t._1, t._2, t._3)
    case Some(4) =>
      val t = asInstanceOf[Tuple4[Object, Object, Object, Object]]
      Array(t._1, t._2, t._3, t._4)
    case Some(n) if n <= $MaxSpecialized =>
      $toArray(this, n)
    case Some(n) =>
      asInstanceOf[TupleXXL].elems
    case None =>
      error(".toArray cannot be applied to tuple of unknown size")
  }

  rewrite def *: [H] (x: H): H *: this.type = {
    type Result = H *: this.type
    rewrite constValueOpt[BoundedSize[this.type]] match {
      case Some(0) =>
        Tuple1(x).asInstanceOf[Result]
      case Some(1) =>
        Tuple2(x, asInstanceOf[Tuple1[_]]._1).asInstanceOf[Result]
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        Tuple3(x, t._1, t._2).asInstanceOf[Result]
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        Tuple4(x, t._1, t._2, t._3).asInstanceOf[Result]
      case Some(4) =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        Tuple5(x, t._1, t._2, t._3, t._4).asInstanceOf[Result]
      case Some(n) =>
        fromArray[Result]($consArray(x, toArray))
      case _ =>
        error("*: cannot be applied to tuple of unknown size")
    }
  }

  rewrite def ++(that: Tuple): Concat[this.type, that.type] = {
    type Result = Concat[this.type, that.type]
    rewrite constValueOpt[BoundedSize[this.type]] match {
      case Some(0) =>
        that.asInstanceOf[Result]
      case Some(1) =>
        if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
        else (asInstanceOf[Tuple1[_]]._1 *: that).asInstanceOf[Result]
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        rewrite constValue[BoundedSize[that.type]] match {
          case 0 => this.asInstanceOf[Result]
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple3(t._1, t._2, u._1).asInstanceOf[Result]
          case 2 =>
            val u = that.asInstanceOf[Tuple2[_, _]]
            Tuple4(t._1, t._2, u._1, u._2).asInstanceOf[Result]
          case _ =>
            genericConcat[Result](this, that).asInstanceOf[Result]
        }
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        rewrite constValue[BoundedSize[that.type]] match {
          case 0 => this.asInstanceOf[Result]
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[Result]
          case _ =>
            genericConcat[Result](this, that).asInstanceOf[Result]
        }
      case Some(_) =>
        if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
        else genericConcat[Result](this, that).asInstanceOf[Result]
      case None =>
        error("++ cannot be applied to tuple of unknown size")
    }
  }

  rewrite def genericConcat[T <: Tuple](xs: Tuple, ys: Tuple): Tuple =
    fromArray[T](xs.toArray ++ ys.toArray)
}

object Tuple {
  transparent val $MaxSpecialized = 22
  transparent private val XXL = $MaxSpecialized + 1

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

  type Elem[+X <: Tuple, +N] = (X, N) match {
    case (x *: xs, 0) => x
    case (x *: xs, S[n1]) => Elem[xs, n1]
  }

  type Size[+X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  private type XXL = S[$MaxSpecialized.type]

  private type BoundedS[N <: Int] = N match {
    case XXL => XXL
    case _ => S[N]
  }

  private[scala] type BoundedSize[X] <: Int = X match {
    case Unit => 0
    case x *: xs => BoundedSize[xs] match {
      case XXL => XXL
      case _ => S[BoundedSize[xs]]
    }
  }

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

  rewrite def fromArray[T <: Tuple](xs: Array[Object]): T =
    rewrite constValue[BoundedSize[T]] match {
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
}

abstract sealed class NonEmptyTuple extends Tuple {
  import Tuple._

  rewrite def head: Head[this.type] = {
    type Result = Head[this.type]
    val resVal = rewrite constValueOpt[BoundedSize[this.type]] match {
      case Some(1) =>
        val t = asInstanceOf[Tuple1[_]]
        t._1
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        t._1
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        t._1
      case Some(4) =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        t._1
      case Some(n) if n > 4 && n <= $MaxSpecialized =>
        asInstanceOf[Product].productElement(0)
      case Some(n) if n > $MaxSpecialized =>
        val t = asInstanceOf[TupleXXL]
        t.elems(0)
      case None =>
        error(".head cannot be applied to tuple of unknown size")
    }
    resVal.asInstanceOf[Result]
  }

  rewrite def tail: Tail[this.type] = {
    type Result = Tail[this.type]
    rewrite constValueOpt[BoundedSize[this.type]]  match {
      case Some(1) =>
        ().asInstanceOf[Result]
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        Tuple1(t._2).asInstanceOf[Result]
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        Tuple2(t._2, t._3).asInstanceOf[Result]
      case Some(4) =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        Tuple3(t._2, t._3, t._4).asInstanceOf[Result]
      case Some(5) =>
        val t = asInstanceOf[Tuple5[_, _, _, _, _]]
        Tuple4(t._2, t._3, t._4, t._5).asInstanceOf[Result]
      case Some(n) if n > 5 =>
        fromArray[Result](toArray.tail)
      case None =>
        error(".tail cannot be applied to tuple of unknown size")
    }
  }

  rewrite def indexOutOfBounds = error("index out of bounds")

  rewrite def apply(transparent n: Int): Elem[this.type, n.type] = {
    type Result = Elem[this.type, n.type]
    rewrite constValueOpt[BoundedSize[this.type]] match {
      case Some(1) =>
        val t = asInstanceOf[Tuple1[_]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[Result]
          case _ => indexOutOfBounds
        }
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[Result]
          case 1 => t._2.asInstanceOf[Result]
          case _ => indexOutOfBounds
        }
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[Result]
          case 1 => t._2.asInstanceOf[Result]
          case 2 => t._3.asInstanceOf[Result]
          case _ => indexOutOfBounds
        }
      case Some(4) =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[Result]
          case 1 => t._2.asInstanceOf[Result]
          case 2 => t._3.asInstanceOf[Result]
          case 3 => t._4.asInstanceOf[Result]
          case _ => indexOutOfBounds
        }
      case Some(s) if s > 4 && s <= $MaxSpecialized && n >= 0 && n < s =>
        asInstanceOf[Product].productElement(n).asInstanceOf[Result]
      case Some(s) if s > $MaxSpecialized && n >= 0 && n < s =>
        asInstanceOf[TupleXXL].elems(n).asInstanceOf[Result]
      case Some(s) =>
        indexOutOfBounds
      case None =>
        error("selection (...) cannot be applied to tuple of unknown size")
    }
  }
}

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  rewrite def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
