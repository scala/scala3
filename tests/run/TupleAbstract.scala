package test {

import annotation.showAsInfix

class TypeLevel {
  type Tuple

  type Empty <: Tuple
    // in the actual implementation, pick scala.Unit, and have it extend `Tuple`.

  @showAsInfix type *:[+H, +T <: Tuple] <: Tuple

  erased def erasedValue[T]: T = ???
  case class Typed[T](val value: T) { type Type = T }
}

class TupleXXL private (es: Array[Object]) {
  override def toString = elems.mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + elems.deep.hashCode
  override def equals(that: Any) = that match {
    case that: TupleXXL => this.elems.deep.equals(that.elems.deep)
    case _ => false
  }
  def elems: Array[Object] = es
}
object TupleXXL {
  def apply(elems: Array[Object]) = new TupleXXL(elems.clone)
}

object Tuples {
  val typelevel = new TypeLevel
  import typelevel._

  def unit = ().asInstanceOf[Empty]

  private final val MaxSpecialized = 7   // 22 in real life

  private rewrite def _empty: Tuple = erasedValue[Empty]
  private rewrite def _pair[H, T <: Tuple] (x: H, xs: T): Tuple = erasedValue[H *: T]

  private rewrite def _size(xs: Tuple): Int = rewrite xs match {
    case _: Empty => 0
    case _: (_ *: xs1) => _size(erasedValue[xs1]) + 1
  }

  private rewrite def _index(xs: Tuple, n: Int): Any = rewrite xs match {
    case _: (x *: _)   if n == 0 => erasedValue[x]
    case _: (_ *: xs1) if n > 0  => _index(erasedValue[xs1], n - 1)
  }

  private rewrite def _head(xs: Tuple): Any = rewrite xs match {
    case _: (x *: _) => erasedValue[x]
  }

  private rewrite def _tail(xs: Tuple): Tuple = rewrite xs match {
    case _: (_ *: xs1) => erasedValue[xs1]
  }

  private rewrite def _concat(xs: Tuple, ys: Tuple): Tuple = rewrite xs match {
    case _: Empty => ys
    case _: (x1 *: xs1) => _pair(erasedValue[x1], _concat(erasedValue[xs1], ys))
  }

  rewrite def fromArray[T <: Tuple](xs: Array[Object]): T =
    rewrite _size(erasedValue[T]) match {
      case 0 => ().asInstanceOf[T]
      case 1 => Tuple1(xs(0)).asInstanceOf[T]
      case 2 => Tuple2(xs(0), xs(1)).asInstanceOf[T]
      case 3 => Tuple3(xs(0), xs(1), xs(2)).asInstanceOf[T]
      case 4 => Tuple4(xs(0), xs(1), xs(2), xs(3)).asInstanceOf[T]
      case 5 => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4)).asInstanceOf[T]
      case 6 => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5)).asInstanceOf[T]
      case 7 => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6)).asInstanceOf[T]
      case _ => TupleXXL(xs).asInstanceOf[T]
    }

  val emptyArray = Array[Object]()

  rewrite implicit def tupleDeco(xs: Tuple): TupleOps = new TupleOps(xs)

  class TupleOps(val xs: Tuple) extends AnyVal {

    rewrite def toArray: Array[Object] = rewrite _size(xs) match {
      case 0 =>
        emptyArray
      case 1 =>
        val t = xs.asInstanceOf[Tuple1[Object]]
        Array(t._1)
      case 2 =>
        val t = xs.asInstanceOf[Tuple2[Object, Object]]
        Array(t._1, t._2)
      case 3 =>
        val t = xs.asInstanceOf[Tuple3[Object, Object, Object]]
        Array(t._1, t._2, t._3)
      case 4 =>
        val t = xs.asInstanceOf[Tuple4[Object, Object, Object, Object]]
        Array(t._1, t._2, t._3, t._4)
      case 5 =>
        val t = xs.asInstanceOf[Tuple5[Object, Object, Object, Object, Object]]
        Array(t._1, t._2, t._3, t._4, t._5)
      case 6 =>
        val t = xs.asInstanceOf[Tuple6[Object, Object, Object, Object, Object, Object]]
        Array(t._1, t._2, t._3, t._4, t._5, t._6)
      case 7 =>
        val t = xs.asInstanceOf[Tuple7[Object, Object, Object, Object, Object, Object, Object]]
        Array(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
      case _ =>
        xs.asInstanceOf[TupleXXL].elems
    }

    rewrite def *: [H] (x: H): Tuple = {
      erased val resTpe = Typed(_pair(x, xs))
      rewrite _size(xs) match {
        case 0 =>
          Tuple1(x).asInstanceOf[resTpe.Type]
        case 1 =>
          Tuple2(x, xs.asInstanceOf[Tuple1[_]]._1).asInstanceOf[resTpe.Type]
        case 2 =>
          val t = xs.asInstanceOf[Tuple2[_, _]]
          Tuple3(x, t._1, t._2).asInstanceOf[resTpe.Type]
        case 3 =>
          val t = xs.asInstanceOf[Tuple3[_, _, _]]
          Tuple4(x, t._1, t._2, t._3).asInstanceOf[resTpe.Type]
        case 4 =>
          val t = xs.asInstanceOf[Tuple4[_, _, _, _]]
          Tuple5(x, t._1, t._2, t._3, t._4).asInstanceOf[resTpe.Type]
        case n =>
          fromArray[resTpe.Type](prepend(x, toArray))
      }
    }

    private def prepend[H](x: H, elems: Array[Object]): Array[Object] = {
      val elems1 = new Array[Object](elems.length + 1)
      elems1(0) = x.asInstanceOf[Object]
      Array.copy(elems, 0, elems1, 1, elems.length)
      elems1
    }

    rewrite def head: Any = {
      erased val resTpe = Typed(_head(xs))
      val resVal = rewrite _size(xs) match {
        case 1 =>
          val t = xs.asInstanceOf[Tuple1[_]]
          t._1
        case 2 =>
          val t = xs.asInstanceOf[Tuple2[_, _]]
          t._1
        case 3 =>
          val t = xs.asInstanceOf[Tuple3[_, _, _]]
          t._1
        case 4 =>
          val t = xs.asInstanceOf[Tuple4[_, _, _, _]]
          t._1
        case n if n > 4 && n <= MaxSpecialized =>
          xs.asInstanceOf[Product].productElement(0)
        case n if n > MaxSpecialized =>
          val t = xs.asInstanceOf[TupleXXL]
          t.elems(0)
      }
      resVal.asInstanceOf[resTpe.Type]
    }

    rewrite def tail: Any = {
      erased val resTpe = Typed(_tail(xs))
      rewrite _size(xs) match {
        case 1 =>
          unit
        case 2 =>
          val t = xs.asInstanceOf[Tuple2[_, _]]
          Tuple1(t._2).asInstanceOf[resTpe.Type]
        case 3 =>
          val t = xs.asInstanceOf[Tuple3[_, _, _]]
          Tuple2(t._2, t._3).asInstanceOf[resTpe.Type]
        case 4 =>
          val t = xs.asInstanceOf[Tuple4[_, _, _, _]]
          Tuple3(t._2, t._3, t._4).asInstanceOf[resTpe.Type]
        case 5 =>
          val t = xs.asInstanceOf[Tuple5[_, _, _, _, _]]
          Tuple4(t._2, t._3, t._4, t._5).asInstanceOf[resTpe.Type]
        case n if n > 5 =>
          fromArray[resTpe.Type](toArray.tail)
      }
    }

    rewrite def apply(n: Int): Any = {
      erased val resTpe = Typed(_index(xs, n))
      rewrite _size(xs) match {
        case 1 =>
          val t = xs.asInstanceOf[Tuple1[_]]
          rewrite n match {
            case 0 => t._1.asInstanceOf[resTpe.Type]
          }
        case 2 =>
          val t = xs.asInstanceOf[Tuple2[_, _]]
          rewrite n match {
            case 0 => t._1.asInstanceOf[resTpe.Type]
            case 1 => t._2.asInstanceOf[resTpe.Type]
          }
        case 3 =>
          val t = xs.asInstanceOf[Tuple3[_, _, _]]
          rewrite n match {
            case 0 => t._1.asInstanceOf[resTpe.Type]
            case 1 => t._2.asInstanceOf[resTpe.Type]
            case 2 => t._3.asInstanceOf[resTpe.Type]
          }
        case 4 =>
          val t = xs.asInstanceOf[Tuple4[_, _, _, _]]
          rewrite n match {
            case 0 => t._1.asInstanceOf[resTpe.Type]
            case 1 => t._2.asInstanceOf[resTpe.Type]
            case 2 => t._3.asInstanceOf[resTpe.Type]
            case 3 => t._4.asInstanceOf[resTpe.Type]
          }
        case s if s > 4 && s <= MaxSpecialized && n >= 0 && n < s =>
          xs.asInstanceOf[Product].productElement(n).asInstanceOf[resTpe.Type]
        case s if s > MaxSpecialized && n >= 0 && n < s =>
          xs.asInstanceOf[TupleXXL].elems(n).asInstanceOf[resTpe.Type]
      }
    }

    rewrite def ++(ys: Tuple): Tuple = {
      erased val resTpe = Typed(_concat(xs, ys))
      rewrite _size(xs) match {
        case 0 => ys
        case 1 =>
          if (_size(ys) == 0) xs
          else xs.head *: ys
        case 2 =>
          val t = xs.asInstanceOf[Tuple2[_, _]]
          rewrite _size(ys) match {
            case 0 => xs
            case 1 =>
              val u = ys.asInstanceOf[Tuple1[_]]
              Tuple3(t._1, t._2, u._1).asInstanceOf[resTpe.Type]
            case 2 =>
              val u = ys.asInstanceOf[Tuple2[_, _]]
              Tuple4(t._1, t._2, u._1, u._2).asInstanceOf[resTpe.Type]
            case _ =>
              genericConcat[resTpe.Type](xs, ys)
          }
        case 3 =>
          val t = xs.asInstanceOf[Tuple3[_, _, _]]
          rewrite _size(ys) match {
            case 0 => xs
            case 1 =>
              val u = ys.asInstanceOf[Tuple1[_]]
              Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[resTpe.Type]
            case _ =>
              genericConcat[resTpe.Type](xs, ys)
          }
        case _ =>
          if (_size(ys) == 0) xs
          else genericConcat[resTpe.Type](xs, ys)
      }
    }

    rewrite def genericConcat[T <: Tuple](xs: Tuple, ys: Tuple): Tuple =
      fromArray[T](xs.toArray ++ ys.toArray)
  }
}
}
object Test extends App {
  import test._
  import Tuples._
  import typelevel._
  val x0 = unit; println(x0)
  val x1 = 1 *: x0; println(x1)
  val x2 = "A" *: x1; println(x2)
  val x3 = 2 *: x2; println(x3)
  val x4 = "B" *: x3; println(x4)
  val x5 = 3 *: x4; println(x5)
  val x6 = "C" *: x5; println(x6)
  val x7 = 4 *: x6; println(x7)
  val x8 = "D" *: x7; println(x8)
  val h1 = x1.head; val h1c: Int = h1; println(s"h1 = $h1")
  val h2 = x2.head; val h2c: String = h2; println(s"h2 = $h2")
  val h7 = x7.head; val h7c: Int = h7; println(s"h7 = $h7")
  val h8 = x8.head; val h8c: String = h8; println(s"h8 = $h8")
  val t1 = x1.tail; val t1c: Empty = t1; println(s"t1 = $t1")
  val t2 = x2.tail; val t2c: Int *: Empty = t2; println(s"t2 = $t2")
  val t7 = x7.tail; val t7c: String *: Int *: Empty = t7.tail.tail.tail.tail; println(s"t7 = $t7")
  val t8 = x8.tail; val t8c: Int = t8(6); println(s"t8 = $t8")
  val a1_0 = x1(0); val a1_0c: Int = a1_0; println(s"a1_0 = $a1_0")
  val a2_0 = x2(0); val a2_0c: String = a2_0; println(s"a2_0 = $a2_0")
  val a3_1 = x3(1); val a3_1c: String = a3_1; println(s"a3_1 = $a3_1")
  val a4_3 = x4(3); val a4_3c: Int = a4_3; println(s"a4_3 = $a4_3")
  val a6_4 = x6(4); val a6_4c: String = a6_4; println(s"a6_4 = $a6_4")
  val a8_0 = x8(0); val a8_0c: String = a8_0; println(s"a8_0 = $a8_0")
  val c0_0 = x0 ++ x0; val c0_0c: Empty = c0_0; println(s"c0_0 = $c0_0")
  val c0_1 = x0 ++ x1; val c0_1c: Int *: Empty = c0_1c; println(s"c0_1 = $c0_1")
  val c1_0 = x1 ++ x0; val c1_0c: Int *: Empty = c1_0c; println(s"c1_0 = $c1_0")
  val c0_4 = x0 ++ x4; val c0_4c: String *: Int *: String *: Int *: Empty = c0_4; println(s"c0_4 = $c0_4")
  val c4_0 = x4 ++ x0; val c4_0c: String *: Int *: String *: Int *: Empty = c4_0; println(s"c4_0 = $c4_0")
  val c1_1 = x1 ++ x1; val c1_1c: Int *: Int *: Empty = c1_1; println(s"c1_1 = $c1_1")
  val c1_8 = x1 ++ x8; val c1_8c: Int *: String *: Int *: String *: Int *: String *: Int *: String *: Int *: Empty = c1_8; println(s"c1_8 = $c1_8")
  val c2_1 = x2 ++ x1; val c2_1c: String *: Int *: Int *: Empty = c2_1; println(s"c2_1 = $c2_1")
  val c2_2 = x2 ++ x2; val c2_2c: String *: Int *: String *: Int *: Empty = c2_2; println(s"c2_2 = $c2_2")
  val c2_3 = x2 ++ x3; val c2_3c: String *: Int *: Int *: String *: Int *: Empty = c2_3; println(s"c2_3 = $c2_3")
  val c3_3 = x3 ++ x3; val c3_3c: Int *: String *: Int *: Int *: String *: Int *: Empty = c3_3; println(s"c3_3 = $c3_3")
}
