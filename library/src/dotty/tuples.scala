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

      case ()                                 => new scala.Tuple1(h)
      case t: scala.Tuple1[_]                 => new scala.Tuple2(h, t._1)
      case t: scala.Tuple2[_, _]              => new scala.Tuple3(h, t._1, t._2)
      case t: scala.Tuple3[_, _, _]           => new scala.Tuple4(h, t._1, t._2, t._3)
      case t: scala.Tuple4[_, _, _, _]        => new scala.Tuple5(h, t._1, t._2, t._3, t._4)
      case t: scala.Tuple5[_, _, _, _, _]     => new scala.Tuple6(h, t._1, t._2, t._3, t._4, t._5)
      case t: scala.Tuple6[_, _, _, _, _, _]  => new scala.Tuple7(h, t._1, t._2, t._3, t._4, t._5, t._6)
        val a = new Array[Any](7)
        a(0) = h
        a(1) = t._1
        a(2) = t._2
        a(3) = t._3
        a(4) = t._4
        a(5) = t._5
        a(6) = t._6
        new LargeTuple(a): Any
    }).asInstanceOf[TupleCons[H, T]]

  def unapply[H, T <: Tuple](t: TupleCons[H, T]): Pair[H, T] =
    ((t: Any) match {
      case impln: LargeTuple[_, _] =>
        val u = impln.underlying
        var s = u.size
        val tail =
          if (s == 7) {
            new scala.Tuple6(u(1), u(2), u(3), u(4), u(5), u(6)).asInstanceOf[T]
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

      case t: scala.Tuple1[_]                 => new Pair(t._1, ())
      case t: scala.Tuple2[_, _]              => new Pair(t._1, new scala.Tuple1(t._2))
      case t: scala.Tuple3[_, _, _]           => new Pair(t._1, new scala.Tuple2(t._2, t._3))
      case t: scala.Tuple4[_, _, _, _]        => new Pair(t._1, new scala.Tuple3(t._2, t._3, t._4))
      case t: scala.Tuple5[_, _, _, _, _]     => new Pair(t._1, new scala.Tuple4(t._2, t._3, t._4, t._5))
      case t: scala.Tuple6[_, _, _, _, _, _]  => new Pair(t._1, new scala.Tuple5(t._2, t._3, t._4, t._5, t._6))
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
