package scala

import language.experimental.captureChecking
import annotation.{showAsInfix, targetName}
import compiletime.*
import compiletime.ops.int.*

/** Tuple of arbitrary arity. */
sealed trait Tuple extends Product {
  import Tuple.*

  /** Creates a copy of this tuple as an Array. */
  inline def toArray: Array[Object] =
    runtime.Tuples.toArray(this)

  /** Creates a copy of this tuple as a List. */
  inline def toList: List[Union[this.type]] =
    this.productIterator.toList
      .asInstanceOf[List[Union[this.type]]]

  /** Creates a copy of this tuple as an IArray. */
  inline def toIArray: IArray[Object] =
    runtime.Tuples.toIArray(this)

  /** Returns a copy of `this` tuple with an element appended. */
  inline def :* [This >: this.type <: Tuple, L] (x: L): This :* L =
    runtime.Tuples.append(x, this).asInstanceOf[This :* L]

  /** Returns a new tuple by prepending the element to `this` tuple.
   *  This operation is O(this.size)
   */
  inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This =
    runtime.Tuples.cons(x, this).asInstanceOf[H *: This]

  /** Gets the i-th element of this tuple.
   *  Equivalent to productElement but with a precise return type.
   */
  inline def apply[This >: this.type <: Tuple](n: Int): Elem[This, n.type] =
    runtime.Tuples.apply(this, n).asInstanceOf[Elem[This, n.type]]

  /** Gets the head of this tuple. */
  inline def head[This >: this.type <: Tuple]: Head[This] =
    runtime.Tuples.apply(this, 0).asInstanceOf[Head[This]]

  /** Gets the initial part of the tuple without its last element. */
  inline def init[This >: this.type <: Tuple]: Init[This] =
    runtime.Tuples.init(this).asInstanceOf[Init[This]]

  /** Gets the last of this tuple. */
  inline def last[This >: this.type <: Tuple]: Last[This] =
    runtime.Tuples.last(this).asInstanceOf[Last[This]]

  /** Gets the tail of this tuple.
   *  This operation is O(this.size)
   */
  inline def tail[This >: this.type <: Tuple]: Tail[This] =
    runtime.Tuples.tail(this).asInstanceOf[Tail[This]]

  /** Returns a new tuple by concatenating `this` tuple with `that` tuple.
   *  This operation is O(this.size + that.size)
   */
  inline def ++ [This >: this.type <: Tuple](that: Tuple): This ++ that.type =
    runtime.Tuples.concat(this, that).asInstanceOf[This ++ that.type]

  /** Returns the size (or arity) of the tuple. */
  inline def size[This >: this.type <: Tuple]: Size[This] =
    runtime.Tuples.size(this).asInstanceOf[Size[This]]

  /** Given two tuples, `(a1, ..., an)` and `(b1, ..., bn)`, returns a tuple
   *  `((a1, b1), ..., (an, bn))`. If the two tuples have different sizes,
   *  the extra elements of the larger tuple will be disregarded.
   *  The result is typed as `((A1, B1), ..., (An, Bn))` if at least one of the
   *  tuple types has a `EmptyTuple` tail. Otherwise the result type is
   *  `(A1, B1) *: ... *: (Ai, Bi) *: Tuple`
   */
  inline def zip[This >: this.type <: Tuple, T2 <: Tuple](t2: T2): Zip[This, T2] =
    runtime.Tuples.zip(this, t2).asInstanceOf[Zip[This, T2]]

  /** Called on a tuple `(a1, ..., an)`, returns a new tuple `(f(a1), ..., f(an))`.
   *  The result is typed as `(F[A1], ..., F[An])` if the tuple type is fully known.
   *  If the tuple is of the form `a1 *: ... *: Tuple` (that is, the tail is not known
   *  to be the cons type.
   */
  inline def map[F[_]](f: [t] => t => F[t]): Map[this.type, F] =
    runtime.Tuples.map(this, f).asInstanceOf[Map[this.type, F]]

  /** Given a tuple `(a1, ..., am)`, returns the tuple `(a1, ..., an)` consisting
   *  of its first n elements.
   */
  inline def take[This >: this.type <: Tuple](n: Int): Take[This, n.type] =
    runtime.Tuples.take(this, n).asInstanceOf[Take[This, n.type]]


  /** Given a tuple `(a1, ..., am)`, returns the tuple `(an+1, ..., am)` consisting
   *  all its elements except the first n ones.
   */
  inline def drop[This >: this.type <: Tuple](n: Int): Drop[This, n.type] =
    runtime.Tuples.drop(this, n).asInstanceOf[Drop[This, n.type]]

  /** Given a tuple `(a1, ..., am)`, returns a pair of the tuple `(a1, ..., an)`
   *  consisting of the first n elements, and the tuple `(an+1, ..., am)` consisting
   *  of the remaining elements.
   */
  inline def splitAt[This >: this.type <: Tuple](n: Int): Split[This, n.type] =
    runtime.Tuples.splitAt(this, n).asInstanceOf[Split[This, n.type]]

  /** Given a tuple `(a1, ..., am)`, returns the reversed tuple `(am, ..., a1)`
   *  consisting all its elements.
   */
  inline def reverse[This >: this.type <: Tuple]: Reverse[This] =
    runtime.Tuples.reverse(this).asInstanceOf[Reverse[This]]
}

object Tuple {

  /** Type of a tuple with an element appended. */
  type Append[X <: Tuple, Y] <: NonEmptyTuple = X match {
    case EmptyTuple => Y *: EmptyTuple
    case x *: xs => x *: Append[xs, Y]
  }

  /** An infix shorthand for `Append[X, Y]`. */
  infix type :*[X <: Tuple, Y] = Append[X, Y]

  /** Type of the head of a tuple. */
  type Head[X <: Tuple] = X match {
    case x *: _ => x
  }

  /** Type of the initial part of the tuple without its last element. */
  type Init[X <: Tuple] <: Tuple = X match {
    case _ *: EmptyTuple => EmptyTuple
    case x *: xs =>
      x *: Init[xs]
  }

  /** Type of the tail of a tuple. */
  type Tail[X <: Tuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  /** Type of the last element of a tuple. */
  type Last[X <: Tuple] = X match {
    case x *: EmptyTuple => x
    case _ *: xs => Last[xs]
  }

  /** Type of the concatenation of two tuples. */
  type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case EmptyTuple => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  /** An infix shorthand for `Concat[X, Y]`. */
  infix type ++[X <: Tuple, +Y <: Tuple] = Concat[X, Y]

  /** Type of the element at position N in the tuple X. */
  type Elem[X <: Tuple, N <: Int] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  /** Literal constant Int size of a tuple. */
  type Size[X <: Tuple] <: Int = X match {
    case EmptyTuple => 0
    case x *: xs => S[Size[xs]]
  }

  /** Folds a tuple `(T1, ..., Tn)` into `F[T1, F[... F[Tn, Z]...]]]`. */
  type Fold[Tup <: Tuple, Z, F[_, _]] = Tup match
    case EmptyTuple => Z
    case h *: t => F[h, Fold[t, Z, F]]

  /** Converts a tuple `(T1, ..., Tn)` to `(F[T1], ..., F[Tn])`. */
  type Map[Tup <: Tuple, F[_ <: Union[Tup]]] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => F[h] *: Map[t, F]
  }

  /** Converts a tuple `(T1, ..., Tn)` to a flattened `(..F[T1], ..., ..F[Tn])`. */
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
  type Filter[Tup <: Tuple, P[_ <: Union[Tup]] <: Boolean] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => P[h] match {
      case true => h *: Filter[t, P]
      case false => Filter[t, P]
    }
  }

  /** Given two tuples, `A1 *: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
   *  where at least one of `At` or `Bt` is `EmptyTuple`,
   *  returns the tuple type `(A1, B1) *: ... *: (An, Bn) *: EmptyTuple`.
   */
  type Zip[T1 <: Tuple, T2 <: Tuple] <: Tuple = (T1, T2) match {
    case (h1 *: t1, h2 *: t2) => (h1, h2) *: Zip[t1, t2]
    case _ => EmptyTuple
  }

  /** Converts a tuple `(F[T1], ..., F[Tn])` to `(T1,  ... Tn)`. */
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

  /** Type of the reversed tuple. */
  type Reverse[X <: Tuple] = ReverseOnto[X, EmptyTuple]

  /** Prepends all elements of a tuple in reverse order onto the other tuple. */
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

  /** A type level Boolean indicating whether the tuple `X` has an element
   *  that matches `Y`.
   *  @pre  The elements of `X` are assumed to be singleton types
   */
  type Contains[X <: Tuple, Y] <: Boolean = X match
    case Y *: _ => true
    case _ *: xs => Contains[xs, Y]
    case EmptyTuple => false

  /** A type level Boolean indicating whether the type `Y` contains
   *  none of the elements of `X`.
   *  @pre  The elements of `X` and `Y` are assumed to be singleton types
   */
  type Disjoint[X <: Tuple, Y <: Tuple] <: Boolean = X match
    case x *: xs => Contains[Y, x] match
      case true => false
      case false => Disjoint[xs, Y]
    case EmptyTuple => true

  /** Empty tuple. */
  def apply(): EmptyTuple = EmptyTuple

  /** Tuple with one element. */
  def apply[T](x: T): T *: EmptyTuple = Tuple1(x)

  /** Uniform tuple construction for any arity with type preservation.
   *  For arities 0 and 1, the overloads above are preferred (binary compatibility).
   *  For arities >= 2, this varargs version is used.
   *
   *  Examples:
   *  {{{
   *  Tuple()              // EmptyTuple
   *  Tuple(1)             // Tuple1[Int]
   *  Tuple(1, "a")        // (Int, String)
   *  Tuple(1, "a", 2.0)   // (Int, String, Double)
   *  }}}
   */
  transparent inline def apply(inline args: Any*): Tuple = ${ TupleMacros.applyImpl('args) }

  /** Matches an empty tuple. */
  def unapply(x: EmptyTuple): true = true

  /** Matches a 1-tuple, extracting its element. */
  @targetName("unapply1")
  inline def unapply[A](x: Tuple1[A]): Some[A] = Some(x._1)

  /** Matches a 2-tuple, extracting its elements. */
  @targetName("unapply2")
  inline def unapply[A, B](x: (A, B)): (A, B) = x

  /** Matches a 3-tuple, extracting its elements. */
  @targetName("unapply3")
  inline def unapply[A, B, C](x: (A, B, C)): (A, B, C) = x

  /** Matches a 4-tuple, extracting its elements. */
  @targetName("unapply4")
  inline def unapply[A, B, C, D](x: (A, B, C, D)): (A, B, C, D) = x

  /** Matches a 5-tuple, extracting its elements. */
  @targetName("unapply5")
  inline def unapply[A, B, C, D, E](x: (A, B, C, D, E)): (A, B, C, D, E) = x

  /** Matches a 6-tuple, extracting its elements. */
  @targetName("unapply6")
  inline def unapply[A, B, C, D, E, F](x: (A, B, C, D, E, F)): (A, B, C, D, E, F) = x

  /** Matches a 7-tuple, extracting its elements. */
  @targetName("unapply7")
  inline def unapply[A, B, C, D, E, F, G](x: (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) = x

  /** Matches a 8-tuple, extracting its elements. */
  @targetName("unapply8")
  inline def unapply[A, B, C, D, E, F, G, H](x: (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) = x

  /** Matches a 9-tuple, extracting its elements. */
  @targetName("unapply9")
  inline def unapply[A, B, C, D, E, F, G, H, I](x: (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) = x

  /** Matches a 10-tuple, extracting its elements. */
  @targetName("unapply10")
  inline def unapply[A, B, C, D, E, F, G, H, I, J](x: (A, B, C, D, E, F, G, H, I, J)): (A, B, C, D, E, F, G, H, I, J) = x

  /** Matches a 11-tuple, extracting its elements. */
  @targetName("unapply11")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K](x: (A, B, C, D, E, F, G, H, I, J, K)): (A, B, C, D, E, F, G, H, I, J, K) = x

  /** Matches a 12-tuple, extracting its elements. */
  @targetName("unapply12")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L](x: (A, B, C, D, E, F, G, H, I, J, K, L)): (A, B, C, D, E, F, G, H, I, J, K, L) = x

  /** Matches a 13-tuple, extracting its elements. */
  @targetName("unapply13")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M](x: (A, B, C, D, E, F, G, H, I, J, K, L, M)): (A, B, C, D, E, F, G, H, I, J, K, L, M) = x

  /** Matches a 14-tuple, extracting its elements. */
  @targetName("unapply14")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) = x

  /** Matches a 15-tuple, extracting its elements. */
  @targetName("unapply15")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) = x

  /** Matches a 16-tuple, extracting its elements. */
  @targetName("unapply16")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) = x

  /** Matches a 17-tuple, extracting its elements. */
  @targetName("unapply17")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) = x

  /** Matches a 18-tuple, extracting its elements. */
  @targetName("unapply18")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) = x

  /** Matches a 19-tuple, extracting its elements. */
  @targetName("unapply19")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) = x

  /** Matches a 20-tuple, extracting its elements. */
  @targetName("unapply20")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) = x

  /** Matches a 21-tuple, extracting its elements. */
  @targetName("unapply21")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) = x

  /** Matches a 22-tuple, extracting its elements. */
  @targetName("unapply22")
  inline def unapply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](x: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) = x

  /** Fallback for abstract Tuple types - provides runtime arity matching. */
  def unapplySeq(x: Tuple): Option[Seq[Any]] = Some(x.productIterator.toSeq)

  /** Converts an array into a tuple of unknown arity and types. */
  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    runtime.Tuples.fromArray(xs2)
  }

  /** Converts an immutable array into a tuple of unknown arity and types. */
  def fromIArray[T](xs: IArray[T]): Tuple = {
    val xs2: IArray[Object] = xs match {
      case xs: IArray[Object] @unchecked => xs
      case _ =>
        xs.map(_.asInstanceOf[Object])
    }
    runtime.Tuples.fromIArray(xs2)
  }

  /** Converts a Product into a tuple of unknown arity and types. */
  def fromProduct(product: Product): Tuple =
    runtime.Tuples.fromProduct(product)

  def fromProductTyped[P <: Product](p: P)(using m: scala.deriving.Mirror.ProductOf[P]): m.MirroredElemTypes =
    runtime.Tuples.fromProduct(p).asInstanceOf[m.MirroredElemTypes]

  given canEqualEmptyTuple: CanEqual[EmptyTuple, EmptyTuple] = CanEqual.derived
  given canEqualTuple[H1, T1 <: Tuple, H2, T2 <: Tuple](
    using eqHead: CanEqual[H1, H2], eqTail: CanEqual[T1, T2]
  ): CanEqual[H1 *: T1, H2 *: T2] = CanEqual.derived
}

/** A tuple of 0 elements. */
type EmptyTuple = EmptyTuple.type

/** A tuple of 0 elements. */
case object EmptyTuple extends Tuple {
  override def toString(): String = "()"
}

/** Tuple of arbitrary non-zero arity. */
sealed trait NonEmptyTuple extends Tuple

@showAsInfix
sealed abstract class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  def unapply[H, T <: Tuple](x: H *: T): (H, T) = (x.head, x.tail)
}
