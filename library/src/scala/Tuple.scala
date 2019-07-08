package scala
import annotation.showAsInfix
import compiletime._
import internal._

import scala.runtime.DynamicTuple

/** Tuple of arbitrary arity */
sealed trait Tuple extends Any {
  import Tuple._

  /** Create a copy this tuple as an Array */
  inline def toArray: Array[Object] =
    DynamicTuple.dynamicToArray(this)

  /** Return a new tuple by prepending the element to `this` tuple.
   *  This opteration is O(this.size)
   */
  inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This =
    DynamicTuple.dynamicCons[H, This](x, this)

  /** Return a new tuple by concatenating `this` tuple with `that` tuple.
   *  This opteration is O(this.size + that.size)
   */
  inline def ++ [This >: this.type <: Tuple](that: Tuple): Concat[This, that.type] =
    DynamicTuple.dynamicConcat[This, that.type](this, that)

  /** Return the size (or arity) of the tuple */
  inline def size[This >: this.type <: Tuple]: Size[This] =
    DynamicTuple.dynamicSize(this)

}

object Tuple {

  /** Type of the head of a tuple */
  type Head[X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  /** Type of the tail of a tuple */
  type Tail[X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  /** Type of the concatenation of two tuples */
  type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  /** Type of the element a position N in the tuple X */
  type Elem[X <: Tuple, N <: Int] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  /** Literal constant Int size of a tuple */
  type Size[X <: Tuple] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  /** Converts a tuple `(T1, ..., Tn)` to `(F[T1], ..., F[Tn])` */
  type Map[Tup <: Tuple, F[_]] <: Tuple = Tup match {
    case Unit => Unit
    case h *: t => F[h] *: Map[t, F]
  }

  /** Convert an array into a tuple of unknown arity and types */
  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    DynamicTuple.dynamicFromArray[Tuple](xs2)
  }

  /** Convert a Product into a tuple of unknown arity and types */
  def fromProduct(product: Product): Tuple =
    runtime.DynamicTuple.dynamicFromProduct[Tuple](product)

}

/** Tuple of arbitrary non-zero arity */
sealed trait NonEmptyTuple extends Tuple {
  import Tuple._

  /** Get the i-th element of this tuple.
   *  Equivalent to productElement but with a precise return type.
   */
  inline def apply[This >: this.type <: NonEmptyTuple](n: Int): Elem[This, n.type] =
    DynamicTuple.dynamicApply[This, n.type](this, n)

  /** Get the head of this tuple */
  inline def head[This >: this.type <: NonEmptyTuple]: Head[This] =
    DynamicTuple.dynamicApply[This, 0](this, 0)

  /** Get the tail of this tuple.
   *  This opteration is O(this.size)
   */
  inline def tail[This >: this.type <: NonEmptyTuple]: Tail[This] =
    DynamicTuple.dynamicTail[This](this)

}

@showAsInfix
sealed abstract class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  inline def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
