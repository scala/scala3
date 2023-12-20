package scala

import annotation.{experimental, showAsInfix}
import compiletime.*
import compiletime.ops.int.*

/** Tuple of arbitrary arity */
sealed trait Tuple extends Product {
  import Tuple.*

  /** Create a copy of this tuple as an Array */
  private[Tuple]
  inline def toArray: Array[Object] =
    runtime.Tuples.toArray(this)

  /** Create a copy of this tuple as a List */
  private[Tuple]
  inline def toList: List[Union[this.type]] =
    this.productIterator.toList
      .asInstanceOf[List[Union[this.type]]]

  /** Create a copy of this tuple as an IArray */
  private[Tuple]
  inline def toIArray: IArray[Object] =
    runtime.Tuples.toIArray(this)

  /** Return a copy of `this` tuple with an element appended */
  private[Tuple]
  inline def :* [This >: this.type <: Tuple, L] (x: L): Append[This, L] =
    runtime.Tuples.append(x, this).asInstanceOf[Append[This, L]]

  /** Return a new tuple by prepending the element to `this` tuple.
   *  This operation is O(this.size)
   */
  private[Tuple]
  inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This =
    runtime.Tuples.cons(x, this).asInstanceOf[H *: This]

  /** Return a new tuple by concatenating `this` tuple with `that` tuple.
   *  This operation is O(this.size + that.size)
   */
  private[Tuple]
  inline def ++ [This >: this.type <: Tuple](that: Tuple): Concat[This, that.type] =
    runtime.Tuples.concat(this, that).asInstanceOf[Concat[This, that.type]]

  /** Return the size (or arity) of the tuple */
  private[Tuple]
  inline def size[This >: this.type <: Tuple]: Size[This] =
    runtime.Tuples.size(this).asInstanceOf[Size[This]]

  /** Given two tuples, `(a1, ..., an)` and `(a1, ..., an)`, returns a tuple
   *  `((a1, b1), ..., (an, bn))`. If the two tuples have different sizes,
   *  the extra elements of the larger tuple will be disregarded.
   *  The result is typed as `((A1, B1), ..., (An, Bn))` if at least one of the
   *  tuple types has a `EmptyTuple` tail. Otherwise the result type is
   *  `(A1, B1) *: ... *: (Ai, Bi) *: Tuple`
   */
  private[Tuple]
  inline def zip[This >: this.type <: Tuple, T2 <: Tuple](t2: T2): Zip[This, T2] =
    runtime.Tuples.zip(this, t2).asInstanceOf[Zip[This, T2]]

  /** Called on a tuple `(a1, ..., an)`, returns a new tuple `(f(a1), ..., f(an))`.
   *  The result is typed as `(F[A1], ..., F[An])` if the tuple type is fully known.
   *  If the tuple is of the form `a1 *: ... *: Tuple` (that is, the tail is not known
   *  to be the cons type.
   */
  private[Tuple]
  inline def map[F[_]](f: [t] => t => F[t]): Map[this.type, F] =
    runtime.Tuples.map(this, f).asInstanceOf[Map[this.type, F]]

  /** Given a tuple `(a1, ..., am)`, returns the tuple `(a1, ..., an)` consisting
   *  of its first n elements.
   */
  private[Tuple]
  inline def take[This >: this.type <: Tuple](n: Int): Take[This, n.type] =
    runtime.Tuples.take(this, n).asInstanceOf[Take[This, n.type]]


  /** Given a tuple `(a1, ..., am)`, returns the tuple `(an+1, ..., am)` consisting
   *  all its elements except the first n ones.
   */
  private[Tuple]
  inline def drop[This >: this.type <: Tuple](n: Int): Drop[This, n.type] =
    runtime.Tuples.drop(this, n).asInstanceOf[Drop[This, n.type]]

  /** Given a tuple `(a1, ..., am)`, returns a pair of the tuple `(a1, ..., an)`
   *  consisting of the first n elements, and the tuple `(an+1, ..., am)` consisting
   *  of the remaining elements.
   */
  private[Tuple]
  inline def splitAt[This >: this.type <: Tuple](n: Int): Split[This, n.type] =
    runtime.Tuples.splitAt(this, n).asInstanceOf[Split[This, n.type]]
}

object Tuple {

  // TODO should it be `extension [H](x: H) def *:(tail: Tuple): H *: tuple.type` ?
  extension [H, Tail <: Tuple](x: H)
    /** Return a new tuple by prepending the element to `tail` tuple.
     *  This operation is O(tail.size)
     */
    def *:(tail: Tail): H *: Tail = runtime.Tuples.cons(x, tail).asInstanceOf[H *: Tail]

  extension (tuple: Tuple)
    /** Get the head of this tuple */
    def head[This >: tuple.type <: Tuple]: Head[This] & Head[tuple.type] =
      runtime.Tuples.apply(tuple, 0).asInstanceOf[Head[This] & Head[tuple.type]]

    /** Get the tail of this tuple.
     *  This operation is O(tuple.size)
     */
    def tail[This >: tuple.type <: Tuple]: Tail[This] & Tail[tuple.type] =
      runtime.Tuples.tail(tuple).asInstanceOf[Tail[This] & Tail[tuple.type]]

    /** Return the size (or arity) of the tuple */
    def size[This >: tuple.type <: Tuple]: Size[This] & Size[tuple.type] =
      runtime.Tuples.size(tuple).asInstanceOf[Size[This] & Size[tuple.type]]

    /** Get the i-th element of this tuple.
     *  Equivalent to productElement but with a precise return type.
     */
    def apply[This >: tuple.type <: Tuple](n: Int): Elem[This, n.type] & Elem[tuple.type, n.type] =
      runtime.Tuples.apply(tuple, n).asInstanceOf[Elem[This, n.type] & Elem[tuple.type, n.type]]

    /** Get the initial part of the tuple without its last element */
    def init[This >: tuple.type <: Tuple]: Init[This] & Init[tuple.type] =
      runtime.Tuples.init(tuple).asInstanceOf[Init[This] & Init[tuple.type]]

    /** Get the last of this tuple */
    def last[This >: tuple.type <: Tuple]: Last[This] & Last[tuple.type] =
      runtime.Tuples.last(tuple).asInstanceOf[Last[This] & Last[tuple.type]]

    /** Return a copy of `tuple` with an element appended */
    def :*[This >: tuple.type <: Tuple, X] (x: X): Append[This, X] & Append[tuple.type, X] =
      runtime.Tuples.append(x, tuple).asInstanceOf[Append[This, X] & Append[tuple.type, X]]

    /** Return a new tuple by concatenating `this` tuple with `that` tuple.
     *  This operation is O(this.size + that.size)
     */
    def ++[This >: tuple.type <: Tuple](that: Tuple): Concat[This, that.type] & Concat[tuple.type, that.type] =
      runtime.Tuples.concat(tuple, that).asInstanceOf[Concat[This, that.type] & Concat[tuple.type, that.type]]

    /** Given a tuple `(a1, ..., am)`, returns the reversed tuple `(am, ..., a1)`
     *  consisting all its elements.
     */
    @experimental
    def reverse[This >: tuple.type <: Tuple]: Reverse[This] & Reverse[tuple.type] =
      runtime.Tuples.reverse(tuple).asInstanceOf[Reverse[This] & Reverse[tuple.type]]

    /** Given two tuples, `(a1, ..., an)` and `(a1, ..., an)`, returns a tuple
     *  `((a1, b1), ..., (an, bn))`. If the two tuples have different sizes,
     *  the extra elements of the larger tuple will be disregarded.
     *  The result is typed as `((A1, B1), ..., (An, Bn))` if at least one of the
     *  tuple types has a `EmptyTuple` tail. Otherwise the result type is
     *  `(A1, B1) *: ... *: (Ai, Bi) *: Tuple`
     */
    // TODO change signature? def zip[That <: Tuple](that: That): Zip[This, tuple.type] & Zip[tuple.type, tuple.type] =
    def zip[This >: tuple.type <: Tuple, That <: Tuple](that: That): Zip[This, That] & Zip[tuple.type, That] =
      runtime.Tuples.zip(tuple, that).asInstanceOf[Zip[This, That] & Zip[tuple.type, That]]

     /** Called on a tuple `(a1, ..., an)`, returns a new tuple `(f(a1), ..., f(an))`.
     *  The result is typed as `(F[A1], ..., F[An])` if the tuple type is fully known.
     *  If the tuple is of the form `a1 *: ... *: Tuple` (that is, the tail is not known
     *  to be the cons type.
     */
    def map[This >: tuple.type <: Tuple, F[_]](f: [t] => t => F[t]): Map[This, F] & Map[tuple.type, F] =
      runtime.Tuples.map(tuple, f).asInstanceOf[Map[This, F] & Map[tuple.type, F]]

    /** Given a tuple `(a1, ..., am)`, returns the tuple `(a1, ..., an)` consisting
     *  of its first n elements.
     */
    def take[This >: tuple.type <: Tuple](n: Int): Take[This, n.type] & Take[tuple.type, n.type] =
      runtime.Tuples.take(tuple, n).asInstanceOf[Take[This, n.type] & Take[tuple.type, n.type]]

    /** Given a tuple `(a1, ..., am)`, returns the tuple `(an+1, ..., am)` consisting
     *  all its elements except the first n ones.
     */
    def drop[This >: tuple.type <: Tuple](n: Int): Drop[This, n.type] & Take[tuple.type, n.type] =
      runtime.Tuples.drop(tuple, n).asInstanceOf[Drop[This, n.type] & Take[tuple.type, n.type]]

    /** Given a tuple `(a1, ..., am)`, returns a pair of the tuple `(a1, ..., an)`
     *  consisting of the first n elements, and the tuple `(an+1, ..., am)` consisting
     *  of the remaining elements.
     */
    def splitAt[This >: tuple.type <: Tuple](n: Int): Split[This, n.type] & Split[tuple.type, n.type] =
      runtime.Tuples.splitAt(tuple, n).asInstanceOf[Split[This, n.type] & Split[tuple.type, n.type]]

    /** Create a copy of this tuple as a List */
    def toList[This >: tuple.type <: Tuple]: List[Union[This]] & List[Union[tuple.type]] =
      tuple.productIterator.toList.asInstanceOf[List[Union[This]] & List[Union[tuple.type]]]
  end extension

  extension (tuple: Tuple)
    /** Create a copy of this tuple as an Array */
    def toArray: Array[AnyRef] = runtime.Tuples.toArray(tuple)

    /** Create a copy of this tuple as an IArray */
    def toIArray: IArray[AnyRef] = runtime.Tuples.toIArray(tuple)
  end extension

  /** Type of a tuple with an element appended */
  type Append[X <: Tuple, Y] <: NonEmptyTuple = X match {
    case EmptyTuple => Y *: EmptyTuple
    case x *: xs => x *: Append[xs, Y]
  }

  /** Type of the head of a tuple */
  type Head[X <: Tuple] = X match {
    case x *: _ => x
    case EmptyTuple => Nothing
  }

  /** Type of the initial part of the tuple without its last element */
  type Init[X <: Tuple] <: Tuple = X match {
    case _ *: EmptyTuple => EmptyTuple
    case x *: xs => x *: Init[xs]
    case EmptyTuple => Nothing
  }

  /** Type of the tail of a tuple */
  type Tail[X <: Tuple] <: Tuple = X match {
    case _ *: xs => xs
    case EmptyTuple => Nothing
  }

  /** Type of the last element of a tuple */
  type Last[X <: Tuple] = X match {
    case x *: EmptyTuple => x
    case _ *: xs => Last[xs]
    case EmptyTuple => Nothing
  }

  /** Type of the concatenation of two tuples */
  type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case EmptyTuple => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  /** Type of the element at position N in the tuple X */
  type Elem[X <: Tuple, N <: Int] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  /** Literal constant Int size of a tuple */
  type Size[X <: Tuple] <: Int = X match {
    case EmptyTuple => 0
    case x *: xs => S[Size[xs]]
  }

  /** Fold a tuple `(T1, ..., Tn)` into `F[T1, F[... F[Tn, Z]...]]]` */
  type Fold[Tup <: Tuple, Z, F[_, _]] = Tup match
    case EmptyTuple => Z
    case h *: t => F[h, Fold[t, Z, F]]

  /** Converts a tuple `(T1, ..., Tn)` to `(F[T1], ..., F[Tn])` */
  type Map[Tup <: Tuple, F[_ <: Union[Tup]]] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => F[h] *: Map[t, F]
  }

  /** Converts a tuple `(T1, ..., Tn)` to a flattened `(..F[T1], ..., ..F[Tn])` */
  type FlatMap[Tup <: Tuple, F[_ <: Union[Tup]] <: Tuple] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => Concat[F[h], FlatMap[t, F]]
  }

  /** Filters out those members of the tuple for which the predicate `P` returns `false`.
   *  A predicate `P[X]` is a type that can be either `true` or `false`. For example:
   *  ```scala
   *  type IsString[x] <: Boolean = x match {
   *    case String => true
   *    case _ => false
   *  }
   *  summon[Tuple.Filter[(1, "foo", 2, "bar"), IsString] =:= ("foo", "bar")]
   *  ```
   *  @syntax markdown
   */
  type Filter[Tup <: Tuple, P[_] <: Boolean] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => P[h] match {
      case true => h *: Filter[t, P]
      case false => Filter[t, P]
    }
  }

  /** Given two tuples, `A1 *: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
   *  where at least one of `At` or `Bt` is `EmptyTuple` or `Tuple`,
   *  returns the tuple type `(A1, B1) *: ... *: (An, Bn) *: Ct`
   *  where `Ct` is `EmptyTuple` if `At` or `Bt` is `EmptyTuple`, otherwise `Ct` is `Tuple`.
   */
  // TODO should zip be covariant? type Zip[T1 <: Tuple, +T2 <: Tuple] <: Tuple = ...
  type Zip[T1 <: Tuple, T2 <: Tuple] <: Tuple = (T1, T2) match {
    case (h1 *: t1, h2 *: t2) => (h1, h2) *: Zip[t1, t2]
    case (EmptyTuple, _) => EmptyTuple
    case (_, EmptyTuple) => EmptyTuple
    case _ => Tuple
  }

  /** Converts a tuple `(F[T1], ..., F[Tn])` to `(T1,  ... Tn)` */
  type InverseMap[X <: Tuple, F[_]] <: Tuple = X match {
    case F[x] *: t => x *: InverseMap[t, F]
    case EmptyTuple => EmptyTuple
  }

  /** Implicit evidence. IsMappedBy[F][X] is present in the implicit scope iff
   *  X is a tuple for which each element's type is constructed via `F`. E.g.
   *  (F[A1], ..., F[An]), but not `(F[A1], B2, ..., F[An])` where B2 does not
   *  have the shape of `F[A]`.
   */
  type IsMappedBy[F[_]] = [X <: Tuple] =>> X =:= Map[InverseMap[X, F], F]

  /** Type of the reversed tuple */
  @experimental
  type Reverse[X <: Tuple] = ReverseOnto[X, EmptyTuple]

  /** Prepends all elements of a tuple in reverse order onto the other tuple */
  @experimental
  type ReverseOnto[From <: Tuple, +To <: Tuple] <: Tuple = From match
    case x *: xs => ReverseOnto[xs, x *: To]
    case EmptyTuple => To

  /** Transforms a tuple `(T1, ..., Tn)` into `(T1, ..., Ti)`. */
  type Take[T <: Tuple, N <: Int] <: Tuple = N match {
    case 0 => EmptyTuple
    case S[n1] => T match {
      case EmptyTuple => EmptyTuple
      case x *: xs => x *: Take[xs, n1]
    }
  }

  /** Transforms a tuple `(T1, ..., Tn)` into `(Ti+1, ..., Tn)`. */
  type Drop[T <: Tuple, N <: Int] <: Tuple = N match {
    case 0 => T
    case S[n1] => T match {
      case EmptyTuple => EmptyTuple
      case x *: xs => Drop[xs, n1]
    }
  }

  /** Splits a tuple (T1, ..., Tn) into a pair of two tuples `(T1, ..., Ti)` and
   * `(Ti+1, ..., Tn)`.
   */
  type Split[T <: Tuple, N <: Int] = (Take[T, N], Drop[T, N])

  /** Given a tuple `(T1, ..., Tn)`, returns a union of its
   *  member types: `T1 | ... | Tn`. Returns `Nothing` if the tuple is empty.
   */
  type Union[T <: Tuple] = Fold[T, Nothing, [x, y] =>> x | y]

  /** Empty tuple */
  def apply(): EmptyTuple = EmptyTuple

  /** Tuple with one element */
  def apply[T](x: T): T *: EmptyTuple = Tuple1(x)

  /** Matches an empty tuple. */
  def unapply(x: EmptyTuple): true = true

  /** Convert an array into a tuple of unknown arity and types */
  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    runtime.Tuples.fromArray(xs2)
  }

  /** Convert an immutable array into a tuple of unknown arity and types */
  def fromIArray[T](xs: IArray[T]): Tuple = {
    val xs2: IArray[Object] = xs match {
      case xs: IArray[Object] @unchecked => xs
      case _ =>
        xs.map(_.asInstanceOf[Object])
    }
    runtime.Tuples.fromIArray(xs2)
  }

  /** Convert a Product into a tuple of unknown arity and types */
  def fromProduct(product: Product): Tuple =
    runtime.Tuples.fromProduct(product)

  def fromProductTyped[P <: Product](p: P)(using m: scala.deriving.Mirror.ProductOf[P]): m.MirroredElemTypes =
    runtime.Tuples.fromProduct(p).asInstanceOf[m.MirroredElemTypes]

  given canEqualEmptyTuple: CanEqual[EmptyTuple, EmptyTuple] = CanEqual.derived
  given canEqualTuple[H1, T1 <: Tuple, H2, T2 <: Tuple](
    using eqHead: CanEqual[H1, H2], eqTail: CanEqual[T1, T2]
  ): CanEqual[H1 *: T1, H2 *: T2] = CanEqual.derived
}

/** A tuple of 0 elements */
type EmptyTuple = EmptyTuple.type

/** A tuple of 0 elements. */
case object EmptyTuple extends Tuple {
  override def toString(): String = "()"
}

/** Tuple of arbitrary non-zero arity */
sealed trait NonEmptyTuple extends Tuple {
  import Tuple.*

  /** Get the i-th element of this tuple.
   *  Equivalent to productElement but with a precise return type.
   */
  private[NonEmptyTuple]
  inline def apply[This >: this.type <: NonEmptyTuple](n: Int): Elem[This, n.type] =
    runtime.Tuples.apply(this, n).asInstanceOf[Elem[This, n.type]]

  /** Get the head of this tuple */
  private[NonEmptyTuple]
  inline def head[This >: this.type <: NonEmptyTuple]: Head[This] =
    runtime.Tuples.apply(this, 0).asInstanceOf[Head[This]]

  /** Get the initial part of the tuple without its last element */
  private[NonEmptyTuple]
  inline def init[This >: this.type <: NonEmptyTuple]: Init[This] =
    runtime.Tuples.init(this).asInstanceOf[Init[This]]

  /** Get the last of this tuple */
  private[NonEmptyTuple]
  inline def last[This >: this.type <: NonEmptyTuple]: Last[This] =
    runtime.Tuples.last(this).asInstanceOf[Last[This]]

  /** Get the tail of this tuple.
   *  This operation is O(this.size)
   */
  private[NonEmptyTuple]
  inline def tail[This >: this.type <: NonEmptyTuple]: Tail[This] =
    runtime.Tuples.tail(this).asInstanceOf[Tail[This]]
}

@showAsInfix
sealed abstract class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  def unapply[H, T <: Tuple](x: H *: T): (H, T) = (x.head, x.tail)
}
