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

  /** Create a copy this tuple as an IArray */
  inline def toIArray: IArray[Object] =
    DynamicTuple.dynamicToIArray(this)

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

  /** Given two tuples, `(a1, ..., an)` and `(a1, ..., an)`, returns a tuple
   *  `((a1, b1), ..., (an, bn))`. If the two tuples have different sizes,
   *  the extra elements of the larger tuple will be disregarded.
   *  The result is typed as `((A1, B1), ..., (An, Bn))` if at least one of the
   *  tuple types has a `Unit` tail. Otherwise the result type is
   *  `(A1, B1) *: ... *: (Ai, Bi) *: Tuple`
   */
  inline def zip[This >: this.type <: Tuple, T2 <: Tuple](t2: T2): Zip[This, T2] =
    DynamicTuple.dynamicZip(this, t2)

  /** Called on a tuple `(a1, ..., an)`, returns a new tuple `(f(a1), ..., f(an))`.
   *  The result is typed as `(F[A1], ..., F[An])` if the tuple type is fully known.
   *  If the tuple is of the form `a1 *: ... *: Tuple` (that is, the tail is not known
   *  to be the cons type.
   */
  inline def map[F[_]](f: [t] => t => F[t]): Map[this.type, F] =
    DynamicTuple.dynamicMap(this, f)

  /** Given a tuple `(a1, ..., am)`, returns the tuple `(a1, ..., an)` consisting
   *  of its first n elements.
   */
  inline def take[This >: this.type <: Tuple](n: Int): Take[This, n.type] =
    DynamicTuple.dynamicTake[This, n.type](this, n)


  /** Given a tuple `(a1, ..., am)`, returns the tuple `(an+1, ..., am)` consisting
   *  all its elements except the first n ones.
   */
  inline def drop[This >: this.type <: Tuple](n: Int): Drop[This, n.type] =
    DynamicTuple.dynamicDrop[This, n.type](this, n)

  /** Given a tuple `(a1, ..., am)`, returns a pair of the tuple `(a1, ..., an)`
   *  consisting of the first n elements, and the tuple `(an+1, ..., am)` consisting
   *  of the remaining elements.
   */
  inline def splitAt[This >: this.type <: Tuple](n: Int): Split[This, n.type] =
    DynamicTuple.dynamicSplitAt[This, n.type](this, n)
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

  /** Given two tuples, `A1 *: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
   *  where at least one of `At` or `Bt` is `Unit` or `Tuple`,
   *  returns the tuple type `(A1, B1) *: ... *: (An, Bn) *: Ct`
   *  where `Ct` is `Unit` if `At` or `Bt` is `Unit`, otherwise `Ct` is `Tuple`.
   */
  type Zip[T1 <: Tuple, T2 <: Tuple] <: Tuple = (T1, T2) match {
    case (h1 *: t1, h2 *: t2) => (h1, h2) *: Zip[t1, t2]
    case (Unit, _) => Unit
    case (_, Unit) => Unit
    case _ => Tuple
  }

  /** Converts a tuple `(F[T1], ..., F[Tn])` to `(T1,  ... Tn)` */
  type InverseMap[X <: Tuple, F[_]] <: Tuple = X match {
    case F[x] *: t => x *: InverseMap[t, F]
    case Unit => Unit
  }

  /** Implicit evidence. IsMappedBy[F][X] is present in the implicit scope iff
   *  X is a tuple for which each element's type is constructed via `F`. E.g.
   *  (F[A1], ..., F[An]), but not `(F[A1], B2, ..., F[An])` where B2 does not
   *  have the shape of `F[A]`.
   */
  type IsMappedBy[F[_]] = [X <: Tuple] =>> X =:= Map[InverseMap[X, F], F]

  /** Transforms a tuple `(T1, ..., Tn)` into `(T1, ..., Ti)`. */
  type Take[T <: Tuple, N <: Int] <: Tuple = N match {
    case 0 => Unit
    case S[n1] => T match {
      case Unit => Unit
      case x *: xs => x *: Take[xs, n1]
    }
  }

  /** Transforms a tuple `(T1, ..., Tn)` into `(Ti+1, ..., Tn)`. */
  type Drop[T <: Tuple, N <: Int] <: Tuple = N match {
    case 0 => T
    case S[n1] => T match {
      case Unit => Unit
      case x *: xs => Drop[xs, n1]
    }
  }

  /** Splits a tuple (T1, ..., Tn) into a pair of two tuples `(T1, ..., Ti)` and
   * `(Ti+1, ..., Tn)`.
   */
  type Split[T <: Tuple, N <: Int] = (Take[T, N], Drop[T, N])

  /** Convert an array into a tuple of unknown arity and types */
  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    DynamicTuple.dynamicFromArray[Tuple](xs2)
  }

  /** Convert an immutable array into a tuple of unknown arity and types */
  def fromIArray[T](xs: IArray[T]): Tuple = {
    val xs2: IArray[Object] = xs match {
      case xs: IArray[Object] => xs
      case xs =>
        // TODO suport IArray.map
        xs.asInstanceOf[Array[T]].map(_.asInstanceOf[Object]).asInstanceOf[IArray[Object]]
    }
    DynamicTuple.dynamicFromIArray[Tuple](xs2)
  }

  /** Convert a Product into a tuple of unknown arity and types */
  def fromProduct(product: Product): Tuple =
    runtime.DynamicTuple.dynamicFromProduct[Tuple](product)

  def fromProductTyped[P <: Product](p: P)(given m: scala.deriving.Mirror.ProductOf[P]): m.MirroredElemTypes =
    Tuple.fromArray(p.productIterator.toArray).asInstanceOf[m.MirroredElemTypes] // TODO use toIArray of Object to avoid double/triple array copy
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
