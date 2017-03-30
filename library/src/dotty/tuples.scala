package dotty

trait Tuple

trait TupleCons[+H, +T <: Tuple] extends Tuple

case class Pair[+A, +B](a: A, b: B)

object TupleCons {
  def apply[H, T <: Tuple](h: H, t: T): TupleCons[H, T] =
    ((t: Any) match {
      case impln: LargeTuple[_, _] =>
        val underlying = impln.underlying
        var s = underlying.size
        val a = new Array[Any](s + 1)
        a(0) = h
        while (s != 0) {
          a(s) = underlying(s - 1)
          s = s - 1
        }
        new LargeTuple(a)

      // The `: Any` are temporary workaround for #2140
      case ()                                                                              => new scala.Tuple1(h): Any
      case t: scala.Tuple1[_]                                                              => new scala.Tuple2(h, t._1): Any
      case t: scala.Tuple2[_, _]                                                           => new scala.Tuple3(h, t._1, t._2): Any
      case t: scala.Tuple3[_, _, _]                                                        => new scala.Tuple4(h, t._1, t._2, t._3): Any
      case t: scala.Tuple4[_, _, _, _]                                                     => new scala.Tuple5(h, t._1, t._2, t._3, t._4): Any
      case t: scala.Tuple5[_, _, _, _, _]                                                  => new scala.Tuple6(h, t._1, t._2, t._3, t._4, t._5): Any
      case t: scala.Tuple6[_, _, _, _, _, _]                                               => new scala.Tuple7(h, t._1, t._2, t._3, t._4, t._5, t._6): Any
      case t: scala.Tuple7[_, _, _, _, _, _, _]                                            => new scala.Tuple8(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7): Any
      case t: scala.Tuple8[_, _, _, _, _, _, _, _]                                         => new scala.Tuple9(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8): Any
      case t: scala.Tuple9[_, _, _, _, _, _, _, _, _]                                      => new scala.Tuple10(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9): Any
      case t: scala.Tuple10[_, _, _, _, _, _, _, _, _, _]                                  => new scala.Tuple11(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10): Any
      case t: scala.Tuple11[_, _, _, _, _, _, _, _, _, _, _]                               => new scala.Tuple12(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11): Any
      case t: scala.Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]                            => new scala.Tuple13(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12): Any
      case t: scala.Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]                         => new scala.Tuple14(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13): Any
      case t: scala.Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                      => new scala.Tuple15(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14): Any
      case t: scala.Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   => new scala.Tuple16(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15): Any
      case t: scala.Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                => new scala.Tuple17(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16): Any
      case t: scala.Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             => new scala.Tuple18(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17): Any
      case t: scala.Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          => new scala.Tuple19(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18): Any
      case t: scala.Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       => new scala.Tuple20(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19): Any
      case t: scala.Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    => new scala.Tuple21(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20): Any
      case t: scala.Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => new scala.Tuple22(h, t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21): Any
      case t: scala.Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =>
        val a = new Array[Any](23)
        a(0) = h
        a(1) = t._1
        a(2) = t._2
        a(3) = t._3
        a(4) = t._4
        a(5) = t._5
        a(6) = t._6
        a(7) = t._7
        a(8) = t._8
        a(9) = t._9
        a(10) = t._10
        a(11) = t._11
        a(12) = t._12
        a(13) = t._13
        a(14) = t._14
        a(15) = t._15
        a(16) = t._16
        a(17) = t._17
        a(18) = t._18
        a(19) = t._19
        a(20) = t._20
        a(21) = t._21
        a(22) = t._22
        new LargeTuple(a): Any
    }).asInstanceOf[TupleCons[H, T]]

  def unapply[H, T <: Tuple](t: TupleCons[H, T]): Pair[H, T] =
    ((t: Any) match {
      case impln: LargeTuple[_, _] =>
        val u = impln.underlying
        var s = u.size
        val tail =
          if (s == 23) {
            new scala.Tuple22(u(1), u(2), u(3), u(4), u(5), u(6), u(7), u(8), u(9), u(10), u(11), u(12), u(13), u(14), u(15), u(16), u(17), u(18), u(19), u(20), u(21), u(22)).asInstanceOf[T]
          } else {
            s = s - 1
            val a = new Array[Any](s)
            while (s != 0) {
              a(s - 1) = u(s)
              s = s - 1
            }
            new LargeTuple(a)
          }
        val head = u(0)
        new Pair(head, tail)

      // The `: Any` are temporary workaround for #2140
      case t: scala.Tuple1[_]                                                                 => new Pair(t._1, ()): Any
      case t: scala.Tuple2[_, _]                                                              => new Pair(t._1, new scala.Tuple1(t._2)): Any
      case t: scala.Tuple3[_, _, _]                                                           => new Pair(t._1, new scala.Tuple2(t._2, t._3)): Any
      case t: scala.Tuple4[_, _, _, _]                                                        => new Pair(t._1, new scala.Tuple3(t._2, t._3, t._4)): Any
      case t: scala.Tuple5[_, _, _, _, _]                                                     => new Pair(t._1, new scala.Tuple4(t._2, t._3, t._4, t._5)): Any
      case t: scala.Tuple6[_, _, _, _, _, _]                                                  => new Pair(t._1, new scala.Tuple5(t._2, t._3, t._4, t._5, t._6)): Any
      case t: scala.Tuple7[_, _, _, _, _, _, _]                                               => new Pair(t._1, new scala.Tuple6(t._2, t._3, t._4, t._5, t._6, t._7)): Any
      case t: scala.Tuple8[_, _, _, _, _, _, _, _]                                            => new Pair(t._1, new scala.Tuple7(t._2, t._3, t._4, t._5, t._6, t._7, t._8)): Any
      case t: scala.Tuple9[_, _, _, _, _, _, _, _, _]                                         => new Pair(t._1, new scala.Tuple8(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)): Any
      case t: scala.Tuple10[_, _, _, _, _, _, _, _, _, _]                                     => new Pair(t._1, new scala.Tuple9(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)): Any
      case t: scala.Tuple11[_, _, _, _, _, _, _, _, _, _, _]                                  => new Pair(t._1, new scala.Tuple10(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)): Any
      case t: scala.Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]                               => new Pair(t._1, new scala.Tuple11(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)): Any
      case t: scala.Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]                            => new Pair(t._1, new scala.Tuple12(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)): Any
      case t: scala.Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                         => new Pair(t._1, new scala.Tuple13(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)): Any
      case t: scala.Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                      => new Pair(t._1, new scala.Tuple14(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)): Any
      case t: scala.Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   => new Pair(t._1, new scala.Tuple15(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)): Any
      case t: scala.Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                => new Pair(t._1, new scala.Tuple16(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)): Any
      case t: scala.Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             => new Pair(t._1, new scala.Tuple17(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)): Any
      case t: scala.Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          => new Pair(t._1, new scala.Tuple18(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)): Any
      case t: scala.Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       => new Pair(t._1, new scala.Tuple19(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)): Any
      case t: scala.Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    => new Pair(t._1, new scala.Tuple20(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)): Any
      case t: scala.Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => new Pair(t._1, new scala.Tuple21(t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)): Any
    }).asInstanceOf[Pair[H, T]]
}

class LargeTuple[H, T <: Tuple](val underlying: Array[Any]) extends Product with TupleCons[H, T] {
  override def toString: String = underlying.mkString("(", ", ", ")")

  def canEqual(that: Any): Boolean = that.isInstanceOf[LargeTuple[_, _]]
  def productArity: Int = underlying.size
  def productElement(n: Int): Any = underlying(n)

  override def equals(o: Any): Boolean =
    o match {
      case n: LargeTuple[_, _] => n.underlying.sameElements(underlying)
      case _ => false
    }
}

object LargeTuple {
  def wrap[H, T <: Tuple](seq: Seq[Any]): LargeTuple[H, T] =
    new LargeTuple(seq.toArray)
}

object LargeTupleUnapplySeq {
  def unapplySeq[H, T <: Tuple](tuple: Any): Option[Seq[Any]] =
    tuple match {
      case t: LargeTuple[_, _] => Some(t.underlying)
      case _ => None
    }
}

trait  DottyTuple1
object DottyTuple1 {
  def apply[T1](_1: T1): TupleCons[T1, Tuple with Unit] = ???
  def unapply[T1](t: TupleCons[T1, Tuple with Unit]): Option[T1] = ???
}

trait  DottyTuple2
object DottyTuple2 {
  def apply[T1, T2](_1: T1, _2: T2): (T1, T2) = ???
  def unapply[T1, T2](t: (T1, T2)): Option[scala.Product2[T1, T2]] = ???
}

trait  DottyTuple3
object DottyTuple3 {
  def apply[T1, T2, T3](_1: T1, _2: T2, _3: T3): (T1, T2, T3) = ???
  def unapply[T1, T2, T3](t: (T1, T2, T3)): Option[scala.Product3[T1, T2, T3]] = ???
}

trait  DottyTuple4
object DottyTuple4 {
  def apply[T1, T2, T3, T4](_1: T1, _2: T2, _3: T3, _4: T4): (T1, T2, T3, T4) = ???
  def unapply[T1, T2, T3, T4](t: (T1, T2, T3, T4)): Option[scala.Product4[T1, T2, T3, T4]] = ???
}

trait  DottyTuple5
object DottyTuple5 {
  def apply[T1, T2, T3, T4, T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5): (T1, T2, T3, T4, T5) = ???
  def unapply[T1, T2, T3, T4, T5](t: (T1, T2, T3, T4, T5)): Option[scala.Product5[T1, T2, T3, T4, T5]] = ???
}

trait  DottyTuple6
object DottyTuple6 {
  def apply[T1, T2, T3, T4, T5, T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6): (T1, T2, T3, T4, T5, T6) = ???
  def unapply[T1, T2, T3, T4, T5, T6](t: (T1, T2, T3, T4, T5, T6)): Option[scala.Product6[T1, T2, T3, T4, T5, T6]] = ???
}

trait  DottyTuple7
object DottyTuple7 {
  def apply[T1, T2, T3, T4, T5, T6, T7](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7): (T1, T2, T3, T4, T5, T6, T7) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7](t: (T1, T2, T3, T4, T5, T6, T7)): Option[scala.Product7[T1, T2, T3, T4, T5, T6, T7]] = ???
}

trait  DottyTuple8
object DottyTuple8 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8): (T1, T2, T3, T4, T5, T6, T7, T8) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8](t: (T1, T2, T3, T4, T5, T6, T7, T8)): Option[scala.Product8[T1, T2, T3, T4, T5, T6, T7, T8]] = ???
}

trait  DottyTuple9
object DottyTuple9 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9): (T1, T2, T3, T4, T5, T6, T7, T8, T9) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Option[scala.Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = ???
}

trait  DottyTuple10
object DottyTuple10 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)): Option[scala.Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = ???
}

trait  DottyTuple11
object DottyTuple11 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)): Option[scala.Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = ???
}

trait  DottyTuple12
object DottyTuple12 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)): Option[scala.Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = ???
}

trait  DottyTuple13
object DottyTuple13 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)): Option[scala.Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = ???
}

trait  DottyTuple14
object DottyTuple14 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)): Option[scala.Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = ???
}

trait  DottyTuple15
object DottyTuple15 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)): Option[scala.Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = ???
}

trait  DottyTuple16
object DottyTuple16 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)): Option[scala.Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = ???
}

trait  DottyTuple17
object DottyTuple17 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)): Option[scala.Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = ???
}

trait  DottyTuple18
object DottyTuple18 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)): Option[scala.Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = ???
}

trait  DottyTuple19
object DottyTuple19 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)): Option[scala.Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = ???
}

trait  DottyTuple20
object DottyTuple20 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)): Option[scala.Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = ???
}

trait  DottyTuple21
object DottyTuple21 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)): Option[scala.Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = ???
}

trait  DottyTuple22
object DottyTuple22 {
  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21, _22: T22): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) = ???
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)): Option[scala.Product22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = ???
}
