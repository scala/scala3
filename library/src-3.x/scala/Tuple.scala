package scala
import annotation.showAsInfix
import compiletime._
import internal._

import scala.runtime.DynamicTuple

sealed trait Tuple extends Any {
  import Tuple._

  inline def toArray: Array[Object] =
    inline constValueOpt[BoundedSize[this.type]] match {
      case Some(0) =>
        DynamicTuple.empty$Array
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
      case Some(n) if n <= DynamicTuple.MaxSpecialized =>
        DynamicTuple.to$Array(this, n)
      case Some(n) =>
        asInstanceOf[TupleXXL].elems
      case None =>
        DynamicTuple.dynamicToArray(this)
    }

  inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This = {
    type Result = H *: This
    inline constValueOpt[BoundedSize[this.type]] match {
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
        knownTupleFromItrator[H *: this.type](Iterator.single(x) ++ this.asInstanceOf[Product].productIterator)
      case _ =>
        DynamicTuple.dynamic_*:[This, H](this, x)
    }
  }

  inline def ++ [This >: this.type <: Tuple](that: Tuple): Concat[This, that.type] = {
    type Result = Concat[This, that.type]
    inline constValueOpt[BoundedSize[this.type]] match {
      case Some(0) =>
        that.asInstanceOf[Result]
      case Some(1) =>
        if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
        else (asInstanceOf[Tuple1[_]]._1 *: that).asInstanceOf[Result]
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        inline constValue[BoundedSize[that.type]] match {
          case 0 => this.asInstanceOf[Result]
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple3(t._1, t._2, u._1).asInstanceOf[Result]
          case 2 =>
            val u = that.asInstanceOf[Tuple2[_, _]]
            Tuple4(t._1, t._2, u._1, u._2).asInstanceOf[Result]
          case _ =>
            knownTupleFromItrator[Result](this.asInstanceOf[Product].productIterator ++ that.asInstanceOf[Product].productIterator)
        }
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        inline constValue[BoundedSize[that.type]] match {
          case 0 => this.asInstanceOf[Result]
          case 1 =>
            val u = that.asInstanceOf[Tuple1[_]]
            Tuple4(t._1, t._2, t._3, u._1).asInstanceOf[Result]
          case _ =>
            knownTupleFromItrator[Result](this.asInstanceOf[Product].productIterator ++ that.asInstanceOf[Product].productIterator)
      }
      case Some(_) =>
        if (constValue[BoundedSize[that.type]] == 0) this.asInstanceOf[Result]
        else knownTupleFromItrator[Result](this.asInstanceOf[Product].productIterator ++ that.asInstanceOf[Product].productIterator)
      case None =>
        DynamicTuple.dynamic_++[This, that.type](this, that)
    }
  }

  inline def size[This >: this.type <: Tuple]: Size[This] = {
    type Result = Size[This]
    inline constValueOpt[BoundedSize[this.type]] match {
      case Some(n) => n.asInstanceOf[Result]
      case _ => DynamicTuple.dynamicSize(this)
    }
  }

}

object Tuple {

  type Head[X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  type Tail[X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[X <: Tuple, N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Size[X] <: Int = X match {
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

  private[scala] inline def knownTupleFromItrator[T <: Tuple](it: Iterator[Any]): T =
    inline constValue[BoundedSize[T]] match {
      case 0  => ().asInstanceOf[T]
      case 1  => Tuple1(it.next()).asInstanceOf[T]
      case 2  => Tuple2(it.next(), it.next()).asInstanceOf[T]
      case 3  => Tuple3(it.next(), it.next(), it.next()).asInstanceOf[T]
      case 4  => Tuple4(it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 5  => Tuple5(it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 6  => Tuple6(it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 7  => Tuple7(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 8  => Tuple8(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 9  => Tuple9(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 10 => Tuple10(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 11 => Tuple11(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 12 => Tuple12(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 13 => Tuple13(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 14 => Tuple14(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 15 => Tuple15(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 16 => Tuple16(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 17 => Tuple17(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 18 => Tuple18(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 19 => Tuple19(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 20 => Tuple20(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 21 => Tuple21(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case 22 => Tuple22(it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next(), it.next()).asInstanceOf[T]
      case _ => TupleXXL(it.asInstanceOf[Iterator[Object]].toArray).asInstanceOf[T]
    }

  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    DynamicTuple.dynamicFromArray[Tuple](xs2)
  }

  def fromProduct(product: Product): Tuple =
    runtime.DynamicTuple.dynamicFromProduct[Tuple](product)

}

sealed trait NonEmptyTuple extends Tuple {
  import Tuple._

  inline def head[This >: this.type <: NonEmptyTuple]: Head[This] = {
    type Result = Head[This]
    val resVal = inline constValueOpt[BoundedSize[this.type]] match {
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
      case Some(n) if n > 4 && n <= DynamicTuple.MaxSpecialized =>
        asInstanceOf[Product].productElement(0)
      case Some(n) if n > DynamicTuple.MaxSpecialized =>
        val t = asInstanceOf[TupleXXL]
        t.elems(0)
      case None =>
        DynamicTuple.dynamicHead[this.type](this)
    }
    resVal.asInstanceOf[Result]
  }

  inline def tail[This >: this.type <: NonEmptyTuple]: Tail[This] = {
    type Result = Tail[This]
    inline constValueOpt[BoundedSize[this.type]]  match {
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
        val it = this.asInstanceOf[Product].productIterator
        it.next()
        knownTupleFromItrator[Result](it)
      case None =>
        DynamicTuple.dynamicTail[This](this)
    }
  }

  inline def fallbackApply(n: Int) =
    inline constValueOpt[n.type] match {
      case Some(n: Int) => error("index out of bounds: ", n)
      case None => DynamicTuple.dynamicApply[this.type, n.type](this, n)
    }

  inline def apply[This >: this.type <: NonEmptyTuple](n: Int): Elem[This, n.type] = {
    type Result = Elem[This, n.type]
    inline constValueOpt[Size[this.type]] match {
      case Some(1) =>
        val t = asInstanceOf[Tuple1[_]]
        inline constValueOpt[n.type] match {
          case Some(0) => t._1.asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case Some(2) =>
        val t = asInstanceOf[Tuple2[_, _]]
        inline constValueOpt[n.type] match {
          case Some(0) => t._1.asInstanceOf[Result]
          case Some(1) => t._2.asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case Some(3) =>
        val t = asInstanceOf[Tuple3[_, _, _]]
        inline constValueOpt[n.type] match {
          case Some(0) => t._1.asInstanceOf[Result]
          case Some(1) => t._2.asInstanceOf[Result]
          case Some(2) => t._3.asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case Some(4) =>
        val t = asInstanceOf[Tuple4[_, _, _, _]]
        inline constValueOpt[n.type] match {
          case Some(0) => t._1.asInstanceOf[Result]
          case Some(1) => t._2.asInstanceOf[Result]
          case Some(2) => t._3.asInstanceOf[Result]
          case Some(3) => t._4.asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case Some(s) if s > 4 && s <= DynamicTuple.MaxSpecialized =>
        val t = asInstanceOf[Product]
        inline constValueOpt[n.type] match {
          case Some(n) if n >= 0 && n < s => t.productElement(n).asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case Some(s) if s > DynamicTuple.MaxSpecialized =>
        val t = asInstanceOf[TupleXXL]
        inline constValueOpt[n.type] match {
          case Some(n) if n >= 0 && n < s => t.elems(n).asInstanceOf[Result]
          case _ => fallbackApply(n).asInstanceOf[Result]
        }
      case _ => fallbackApply(n).asInstanceOf[Result]
    }
  }

}

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  inline def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
