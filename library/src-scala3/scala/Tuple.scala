package scala
import annotation.showAsInfix
import typelevel._

sealed trait Tuple extends Any {
  import Tuple._
  rewrite def toArray: Array[Object] = rewrite _size(this) match {
    case 0 =>
      $emptyArray
    case 1 =>
      val t = asInstanceOf[Tuple1[Object]]
      Array(t._1)
    case 2 =>
      val t = asInstanceOf[Tuple2[Object, Object]]
      Array(t._1, t._2)
    case 3 =>
      val t = asInstanceOf[Tuple3[Object, Object, Object]]
      Array(t._1, t._2, t._3)
    case 4 =>
      val t = asInstanceOf[Tuple4[Object, Object, Object, Object]]
      Array(t._1, t._2, t._3, t._4)
    case n if n <= $MaxSpecialized =>
      $toArray(this, n)
    case n =>
      asInstanceOf[TupleXXL].elems
  }

  rewrite def *: [H] (x: H): Tuple = {
    erased val resTpe = Typed(_pair(x, this))
    rewrite _size(this) match {
      case 0 =>
        Tuple1(x).asInstanceOf[resTpe.Type]
      case 1 =>
        Tuple2(x, asInstanceOf[Tuple1[_]]._1).asInstanceOf[resTpe.Type]
      case 2 =>
        val t = asInstanceOf[Tuple2[_, _]]
        Tuple3(x, t._1, t._2).asInstanceOf[resTpe.Type]
      case 3 =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        Tuple4(x, t._1, t._2, t._3).asInstanceOf[resTpe.Type]
      case 4 =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        Tuple5(x, t._1, t._2, t._3, t._4).asInstanceOf[resTpe.Type]
      case n =>
        fromArray[resTpe.Type]($consArray(x, toArray))
    }
  }

  rewrite def ++(that: Tuple): Tuple = {
    erased val resTpe = Typed(_concat(this, that))
    rewrite _size(this) match {
      case 0 =>
        that
      case 1 =>
        if (_size(that) == 0) this
        else (asInstanceOf[Tuple1[_]]._1 *: that).asInstanceOf[resTpe.Type]
      case 2 =>
        val t = asInstanceOf[Tuple2[_, _]]
        rewrite _size(that) match {
          case 0 => this
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple3(t._1, t._2, u._1).asInstanceOf[resTpe.Type]
          case 2 =>
            val u = that.asInstanceOf[Tuple2[_, _]]
            Tuple4(t._1, t._2, u._1, u._2).asInstanceOf[resTpe.Type]
          case _ =>
            genericConcat[resTpe.Type](this, that)
        }
      case 3 =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        rewrite _size(that) match {
          case 0 => this
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[resTpe.Type]
          case _ =>
            genericConcat[resTpe.Type](this, that)
        }
      case _ =>
        if (_size(that) == 0) this
        else genericConcat[resTpe.Type](this, that)
    }
  }

  rewrite def genericConcat[T <: Tuple](xs: Tuple, ys: Tuple): Tuple =
    fromArray[T](xs.toArray ++ ys.toArray)
}

object Tuple {
  transparent val $MaxSpecialized = 22

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

  private[scala] rewrite def _pair[H, T <: Tuple] (x: H, xs: T): Tuple =
    erasedValue[H *: T]

  private[scala] rewrite def _size(xs: Tuple): Int =
    rewrite xs match {
      case _: Unit => 0
      case _: *:[_, xs1] => _size(erasedValue[xs1]) + 1
    }

  private[scala] rewrite def _head(xs: Tuple): Any = rewrite xs match {
    case _: (x *: _) => erasedValue[x]
  }

  private[scala] rewrite def _tail(xs: Tuple): Tuple = rewrite xs match {
    case _: (_ *: xs1) => erasedValue[xs1]
  }

  private[scala] rewrite def _index(xs: Tuple, n: Int): Any = rewrite xs match {
    case _: (x *: _)   if n == 0 => erasedValue[x]
    case _: (_ *: xs1) if n > 0  => _index(erasedValue[xs1], n - 1)
  }

  private[scala] rewrite def _concat(xs: Tuple, ys: Tuple): Tuple = rewrite xs match {
    case _: Unit => ys
    case _: (x1 *: xs1) => _pair(erasedValue[x1], _concat(erasedValue[xs1], ys))
  }

  rewrite def fromArray[T <: Tuple](xs: Array[Object]): T =
    rewrite _size(erasedValue[T]) match {
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

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple {
  import Tuple._

  rewrite def head: Any = {
    erased val resTpe = Typed(_head(this))
    val resVal = rewrite _size(this) match {
      case 1 =>
        val t = asInstanceOf[Tuple1[_]]
        t._1
      case 2 =>
        val t = asInstanceOf[Tuple2[_, _]]
        t._1
      case 3 =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        t._1
      case 4 =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        t._1
      case n if n > 4 && n <= $MaxSpecialized =>
        asInstanceOf[Product].productElement(0)
      case n if n > $MaxSpecialized =>
        val t = asInstanceOf[TupleXXL]
        t.elems(0)
    }
    resVal.asInstanceOf[resTpe.Type]
  }

  rewrite def tail: Tuple = {
    erased val resTpe = Typed(_tail(this))
    rewrite _size(this) match {
      case 1 =>
        ()
      case 2 =>
        val t = asInstanceOf[Tuple2[_, _]]
        Tuple1(t._2).asInstanceOf[resTpe.Type]
      case 3 =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        Tuple2(t._2, t._3).asInstanceOf[resTpe.Type]
      case 4 =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        Tuple3(t._2, t._3, t._4).asInstanceOf[resTpe.Type]
      case 5 =>
        val t = asInstanceOf[Tuple5[_, _, _, _, _]]
        Tuple4(t._2, t._3, t._4, t._5).asInstanceOf[resTpe.Type]
      case n if n > 5 =>
        fromArray[resTpe.Type](toArray.tail)
    }
  }

  rewrite def apply(n: Int): Any = {
    erased val resTpe = Typed(_index(this, n))
    rewrite _size(this) match {
      case 1 =>
        val t = asInstanceOf[Tuple1[_]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[resTpe.Type]
        }
      case 2 =>
        val t = asInstanceOf[Tuple2[_, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[resTpe.Type]
          case 1 => t._2.asInstanceOf[resTpe.Type]
        }
      case 3 =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[resTpe.Type]
          case 1 => t._2.asInstanceOf[resTpe.Type]
          case 2 => t._3.asInstanceOf[resTpe.Type]
        }
      case 4 =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        rewrite n match {
          case 0 => t._1.asInstanceOf[resTpe.Type]
          case 1 => t._2.asInstanceOf[resTpe.Type]
          case 2 => t._3.asInstanceOf[resTpe.Type]
          case 3 => t._4.asInstanceOf[resTpe.Type]
        }
      case s if s > 4 && s <= $MaxSpecialized && n >= 0 && n < s =>
        asInstanceOf[Product].productElement(n).asInstanceOf[resTpe.Type]
      case s if s > $MaxSpecialized && n >= 0 && n < s =>
        asInstanceOf[TupleXXL].elems(n).asInstanceOf[resTpe.Type]
    }
  }
}

object *: {
  rewrite def unapply[H, T <: Tuple](x: H *: T) = Some((x.head, x.tail))
}
