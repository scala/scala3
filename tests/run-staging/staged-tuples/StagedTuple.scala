package scala.internal

import scala.quoted.*

import scala.runtime.TupleXXL

import scala.runtime.Tuples.MaxSpecialized

object StagedTuple {
  import Tuple.Concat
  import Tuple.Head
  import Tuple.Tail
  import Tuple.Size
  import Tuple.Elem

  private final val specialize = true

  def toArrayStaged(tup: Expr[Tuple], size: Option[Int])(using Quotes): Expr[Array[Object]] = {
    if (!specialize) '{scala.runtime.Tuples.toArray($tup)}
    else size match {
      case Some(0) =>
        '{Array.emptyObjectArray}
      case Some(1) =>
        tup.as[Tuple1[Object]].bind(t => '{Array($t._1)})
      case Some(2) =>
        tup.as[Tuple2[Object, Object]].bind(t => '{Array($t._1, $t._2)})
      case Some(3) =>
        tup.as[Tuple3[Object, Object, Object]].bind(t => '{Array($t._1, $t._2, $t._3)})
      case Some(4) =>
        tup.as[Tuple4[Object, Object, Object, Object]].bind(t => '{Array($t._1, $t._2, $t._3, $t._4)})
      case Some(n) =>
        '{ ${tup.as[TupleXXL]}.toArray }
      case None =>
        '{scala.runtime.Tuples.toArray($tup)}
    }
  }

  def fromArrayStaged[T <: Tuple : Type](xs: Expr[Array[Object]], size: Option[Int])(using Quotes): Expr[T] = {
    if (!specialize) '{scala.runtime.Tuples.fromArray($xs)}.as[T]
    else xs.bind { xs =>
      val tup: Expr[Any] = size match {
        case Some(0)  => '{Tuple()}
        case Some(1)  => '{Tuple1( $xs(0))}
        case Some(2)  => '{Tuple2( $xs(0), $xs(1))}
        case Some(3)  => '{Tuple3( $xs(0), $xs(1), $xs(2))}
        case Some(4)  => '{Tuple4( $xs(0), $xs(1), $xs(2), $xs(3))}
        case Some(5)  => '{Tuple5( $xs(0), $xs(1), $xs(2), $xs(3), $xs(4))}
        case Some(6)  => '{Tuple6( $xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5))}
        case Some(7)  => '{Tuple7( $xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6))}
        case Some(8)  => '{Tuple8( $xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7))}
        case Some(9)  => '{Tuple9( $xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8))}
        case Some(10) => '{Tuple10($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9))}
        case Some(11) => '{Tuple11($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10))}
        case Some(12) => '{Tuple12($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11))}
        case Some(13) => '{Tuple13($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12))}
        case Some(14) => '{Tuple14($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13))}
        case Some(15) => '{Tuple15($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14))}
        case Some(16) => '{Tuple16($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15))}
        case Some(17) => '{Tuple17($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16))}
        case Some(18) => '{Tuple18($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16), $xs(17))}
        case Some(19) => '{Tuple19($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16), $xs(17), $xs(18))}
        case Some(20) => '{Tuple20($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16), $xs(17), $xs(18), $xs(19))}
        case Some(21) => '{Tuple21($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16), $xs(17), $xs(18), $xs(19), $xs(20))}
        case Some(22) => '{Tuple22($xs(0), $xs(1), $xs(2), $xs(3), $xs(4), $xs(5), $xs(6), $xs(7), $xs(8), $xs(9), $xs(10), $xs(11), $xs(12), $xs(13), $xs(14), $xs(15), $xs(16), $xs(17), $xs(18), $xs(19), $xs(20), $xs(21))}
        case Some(_)  => '{TupleXXL($xs)}
        case None     => '{scala.runtime.Tuples.fromArray($xs)}
      }
      tup.as[T]
    }
  }

  def sizeStaged[Res <: Int : Type](tup: Expr[Tuple], size: Option[Int])(using Quotes): Expr[Res] = {
    val res =
      if (!specialize) '{scala.runtime.Tuples.size($tup)}
      else size match {
        case Some(n) => Expr(n)
        case None => '{scala.runtime.Tuples.size($tup)}
      }
    res.as[Res]
  }

  def headStaged[Tup <: NonEmptyTuple : Type](tup: Expr[Tup], size: Option[Int])(using Quotes): Expr[Head[Tup]] = {
    if (!specialize) '{scala.runtime.Tuples.apply($tup, 0)}.as[Head[Tup]]
    else {
      val resVal = size match {
        case Some(1) =>
          '{${ tup.as[Tuple1[_]] }._1}
        case Some(2) =>
          '{${ tup.as[Tuple2[_, _]] }._1}
        case Some(3) =>
          '{${ tup.as[Tuple3[_, _, _]]}._1}
        case Some(4) =>
          '{${ tup.as[Tuple4[_, _, _, _]] }._1}
        case Some(n) if n > 4 && n <= MaxSpecialized =>
          '{${ tup.as[Product] }.productElement(0)}
        case Some(n) if n > MaxSpecialized =>
          '{${tup.as[TupleXXL] }.elems(0)}
        case None =>
          '{scala.runtime.Tuples.apply($tup, 0)}
      }
      resVal.as[Head[Tup]]
    }
  }

  def tailStaged[Tup <: NonEmptyTuple : Type](tup: Expr[Tup], size: Option[Int])(using Quotes): Expr[Tail[Tup]] = {
    if (!specialize) '{scala.runtime.Tuples.tail($tup)}.as[Tail[Tup]]
    else {
      val res = size match {
        case Some(1) =>
          '{Tuple()}
        case Some(2) =>
          tup.as[Tuple2[_, _]].bind(t => '{Tuple1($t._2)})
        case Some(3) =>
          tup.as[Tuple3[_, _, _]].bind(t => '{Tuple2($t._2, $t._3)})
        case Some(4) =>
          tup.as[Tuple4[_, _, _, _]].bind(t => '{Tuple3($t._2, $t._3, $t._4)})
        case Some(5) =>
          tup.as[Tuple5[_, _, _, _, _]].bind(t => '{Tuple4($t._2, $t._3, $t._4, $t._5)})
        case Some(n) if n > 5 =>
          val arr = toArrayStaged(tup, size)
          fromArrayStaged[Tail[Tup]]('{ $arr.tail }, Some(n - 1))
        case None =>
          '{scala.runtime.Tuples.tail($tup)}
      }
      res.as[Tail[Tup]]
    }
  }

  def applyStaged[Tup <: NonEmptyTuple : Type, N <: Int : Type](tup: Expr[Tup], size: Option[Int], n: Expr[N], nValue: Option[Int])(using Quotes): Expr[Elem[Tup, N]] = {

    if (!specialize) '{scala.runtime.Tuples.apply($tup, $n)}.as[Elem[Tup, N]]
    else {
      def fallbackApply(): Expr[Elem[Tup, N]] = nValue match {
        case Some(n) =>
          quotes.reflect.report.error("index out of bounds: " + n, tup)
          '{ throw new IndexOutOfBoundsException(${Expr(n.toString)}) }
        case None => '{scala.runtime.Tuples.apply($tup, $n)}.as[Elem[Tup, N]]
      }
      val res = size match {
        case Some(1) =>
          val t = tup.as[Tuple1[_]]
          nValue match {
            case Some(0) => '{$t._1}
            case _ => fallbackApply()
          }
        case Some(2) =>
          val t = tup.as[Tuple2[_, _]]
          nValue match {
            case Some(0) => '{$t._1}
            case Some(1) => '{$t._2}
            case _ => fallbackApply()
          }
        case Some(3) =>
          val t = tup.as[Tuple3[_, _, _]]
          nValue match {
            case Some(0) => '{$t._1}
            case Some(1) => '{$t._2}
            case Some(2) => '{$t._3}
            case _ => fallbackApply()
          }
        case Some(4) =>
          val t = tup.as[Tuple4[_, _, _, _]]
          nValue match {
            case Some(0) => '{$t._1}
            case Some(1) => '{$t._2}
            case Some(2) => '{$t._3}
            case Some(3) => '{$t._4}
            case _ => fallbackApply()
          }
        case Some(s) if s > 4 && s <= MaxSpecialized =>
          val t = tup.as[Product]
          nValue match {
            case Some(n) if n >= 0 && n < s => '{$t.productElement(${ Expr(n) })}
            case _ => fallbackApply()
          }
        case Some(s) if s > MaxSpecialized =>
          val t = tup.as[TupleXXL]
          nValue match {
            case Some(n) if n >= 0 && n < s => '{$t.elems(${ Expr(n) })}
            case _ => fallbackApply()
          }
        case _ => fallbackApply()
      }
      res.as[Elem[Tup, N]]
    }
  }

  def consStaged[T <: Tuple & Singleton : Type, H : Type](self: Expr[T], x: Expr[H], tailSize: Option[Int])(using Quotes): Expr[H *: T] =
  if (!specialize) '{scala.runtime.Tuples.cons($x, $self)}.as[H *: T]
  else {
    val res = tailSize match {
      case Some(0) =>
        '{Tuple1($x)}
      case Some(1) =>
        self.as[Tuple1[_]].bind(t => '{Tuple2($x, $t._1)})
      case Some(2) =>
        self.as[Tuple2[_, _]].bind(t => '{Tuple3($x, $t._1, $t._2)})
      case Some(3) =>
        self.as[Tuple3[_, _, _]].bind(t => '{Tuple4($x, $t._1, $t._2, $t._3)})
      case Some(4) =>
        self.as[Tuple4[_, _, _, _]].bind(t => '{Tuple5($x, $t._1, $t._2, $t._3, $t._4)})
      case _ =>
        '{scala.runtime.Tuples.cons($x, $self)}
    }
    res.as[H *: T]
  }

  def concatStaged[Self <: Tuple & Singleton : Type, That <: Tuple & Singleton : Type](self: Expr[Self], selfSize: Option[Int], that: Expr[That], thatSize: Option[Int])(using Quotes): Expr[Concat[Self, That]] = {
    if (!specialize) '{scala.runtime.Tuples.concat($self, $that)}.as[Concat[Self, That]]
    else {
      def genericConcat(xs: Expr[Tuple], ys: Expr[Tuple]): Expr[Tuple] =
        // TODO remove ascriptions when #6126 is fixed
        fromArrayStaged[Tuple]('{${ toArrayStaged(xs, None) } ++ (${ toArrayStaged(ys, None) }: Array[Object])}, None)

      val res = selfSize match {
        case Some(0) =>
          that
        case Some(1) =>
          if (thatSize.contains(0)) self
          else consStaged(that, '{$self.asInstanceOf[Tuple1[_]]._1}, thatSize)
        case Some(2) =>
          val self2 = self.as[Tuple2[_, _]]
          thatSize match {
            case Some(0) => self
            case Some(1) =>
              self2.bind { t =>
                that.as[Tuple1[_]].bind(u => '{Tuple3($t._1, $t._2, $u._1)})
              }
            case Some(2) =>
              self2.bind { t =>
                that.as[Tuple2[_, _]].bind(u => '{Tuple4($t._1, $t._2, $u._1, $u._2)})
              }
            case _ =>
              genericConcat(self, that)
          }
        case Some(3) =>
          val self2 = self.as[Tuple3[_, _, _]]
          thatSize match {
            case Some(0) => self
            case Some(1) =>
              self2.bind { t =>
                that.as[Tuple1[_]].bind(u => '{Tuple4($t._1, $t._2, $t._3, $u._1)})
              }
            case _ =>
              genericConcat(self, that)
          }
        case Some(_) =>
          if (thatSize.contains(0)) self
          else genericConcat(self, that)
        case None =>
          '{scala.runtime.Tuples.concat($self, $that)}
      }
      res.as[Concat[Self, That]]
    }
  }

  private implicit class ExprOps[U: Type](expr: Expr[U]) {

    def as[T: Type](using Quotes): Expr[T] = '{ $expr.asInstanceOf[T] }

    def bind[T: Type](in: Expr[U] => Expr[T])(using Quotes): Expr[T] = '{
      val t: U = $expr
      ${in('t)}
    }
  }
}
