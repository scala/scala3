package scala
import compiletime.ops.boolean.*
import collection.immutable.{SeqMap, ListMap}

import language.experimental.captureChecking

object NamedTuple:

  /** The type to which named tuples get mapped to. For instance,
   *      (name: String, age: Int)
   *  gets mapped to
   *      NamedTuple[("name", "age"), (String, Int)]
   */
  opaque type NamedTuple[N <: Tuple, +V <: Tuple] >: V <: AnyNamedTuple = V

  /** A type which is a supertype of all named tuples. */
  opaque type AnyNamedTuple = Any

  def apply[N <: Tuple, V <: Tuple](x: V): NamedTuple[N, V] = x

  def unapply[N <: Tuple, V <: Tuple](x: NamedTuple[N, V]): Some[V] = Some(x)

  /** A named tuple expression will desugar to a call to `build`. For instance,
   *  `(name = "Lyra", age = 23)` will desugar to `build[("name", "age")]()(("Lyra", 23))`.
   *
   *  @tparam N the tuple of literal string types representing the field names
   */
  inline def build[N <: Tuple]()[V <: Tuple](x: V): NamedTuple[N, V] = x

  extension [V <: Tuple](x: V)
    inline def withNames[N <: Tuple]: NamedTuple[N, V] = x

  import NamedTupleDecomposition.{Names, DropNames}
  export NamedTupleDecomposition.{
    Names, DropNames,
    apply, size, init, head, last, tail, take, drop, splitAt, ++, map, reverse, zip, toList, toArray, toIArray, toSeqMap
  }

  extension [N <: Tuple, V <: Tuple](x: NamedTuple[N, V])

    // ALL METHODS DEPENDING ON `toTuple` MUST BE EXPORTED FROM `NamedTupleDecomposition`
    /** The underlying tuple without the names. */
    inline def toTuple: V = x

    // This intentionally works for empty named tuples as well. I think NonEmptyTuple is a dead end
    // and should be reverted, just like NonEmptyList is also appealing at first, but a bad idea
    // in the end.

    // inline def :* [L] (x: L): NamedTuple[Append[N, ???], Append[V, L] = ???
    // inline def *: [H] (x: H): NamedTuple[??? *: N], H *: V] = ???

  end extension

  /** The size of a named tuple, represented as a literal constant subtype of `Int`. */
  type Size[X <: AnyNamedTuple] = Tuple.Size[DropNames[X]]

  /** The type of the element value at position N in the named tuple X. */
  type Elem[X <: AnyNamedTuple, N <: Int] = Tuple.Elem[DropNames[X], N]

  /** The type of the first element value of a named tuple. */
  type Head[X <: AnyNamedTuple] = Elem[X, 0]

  /** The type of the last element value of a named tuple. */
  type Last[X <: AnyNamedTuple] = Tuple.Last[DropNames[X]]

  /** The type of a named tuple consisting of all elements of named tuple X except the first one. */
  type Tail[X <: AnyNamedTuple] = Drop[X, 1]

  /** The type of the initial part of a named tuple without its last element. */
  type Init[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Init[Names[X]], Tuple.Init[DropNames[X]]]

  /** The type of the named tuple consisting of the first `N` elements of `X`,
   *  or all elements if `N` exceeds `Size[X]`.
   */
  type Take[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Take[Names[X], N], Tuple.Take[DropNames[X], N]]

  /** The type of the named tuple consisting of all elements of `X` except the first `N` ones,
   *  or no elements if `N` exceeds `Size[X]`.
   */
  type Drop[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Drop[Names[X], N], Tuple.Drop[DropNames[X], N]]

  /** The pair type `(Take(X, N), Drop[X, N]). */
  type Split[X <: AnyNamedTuple, N <: Int] = (Take[X, N], Drop[X, N])

  /** The type of the concatenation of two tuples `X` and `Y`. */
  type Concat[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    NamedTuple[Tuple.Concat[Names[X], Names[Y]], Tuple.Concat[DropNames[X], DropNames[Y]]]

  /** The type of the named tuple `X` mapped with the type-level function `F`.
   *  If `X = (n1 : T1, ..., ni : Ti)` then `Map[X, F] = `(n1 : F[T1], ..., ni : F[Ti])`.
   */
  type Map[X <: AnyNamedTuple, F[_ <: Tuple.Union[DropNames[X]]]] =
    NamedTuple[Names[X], Tuple.Map[DropNames[X], F]]

  /** A named tuple with the elements of tuple `X` in reversed order. */
  type Reverse[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Reverse[Names[X]], Tuple.Reverse[DropNames[X]]]

  /** The type of the named tuple consisting of all element values of
   *  named tuple `X` zipped with corresponding element values of
   *  named tuple `Y`. If the two tuples have different sizes,
   *  the extra elements of the larger tuple will be disregarded.
   *  The names of `X` and `Y` at the same index must be the same.
   *  The result tuple keeps the same names as the operand tuples.
   *  For example, if
   *  ```
   *     X = (n1 : S1, ..., ni : Si)
   *     Y = (n1 : T1, ..., nj : Tj)  where j >= i
   *  ```
   *  then
   *  ```
   *     Zip[X, Y] = (n1 : (S1, T1), ..., ni: (Si, Ti))
   *  ```
   *  @syntax markdown
   */
  type Zip[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    Names[X] match
      case Names[Y] =>
        NamedTuple[Names[X], Tuple.Zip[DropNames[X], DropNames[Y]]]

  /** A type specially treated by the compiler to represent all fields of a
   *  class argument `T` as a named tuple. Or, if `T` is already a named tuple,
   *  `From[T]` is the same as `T`.
   */
  type From[T] <: AnyNamedTuple

  /** The type of the empty named tuple. */
  type Empty = NamedTuple[EmptyTuple, EmptyTuple]

  /** The empty named tuple. */
  val Empty: Empty = EmptyTuple

  /** The ordering instance for named tuples. */
  given namedTupleOrdering: [N <: Tuple, V <: Tuple] => (ord: Ordering[V]) => Ordering[NamedTuple[N, V]]:
    def compare(x: NamedTuple[N, V], y: NamedTuple[N, V]): Int =
      ord.compare(x.toTuple, y.toTuple)
end NamedTuple

/** Separate from NamedTuple object so that we can match on the opaque type NamedTuple. */
object NamedTupleDecomposition:
  import NamedTuple.*
  extension [N <: Tuple, V <: Tuple](x: NamedTuple[N, V])
    /** The value (without the name) at index `n` of this tuple.
     *
     *  @param n the zero-based index of the element to retrieve
     */
    inline def apply(n: Int): Elem[NamedTuple[N, V], n.type] =
      x.toTuple.apply(n).asInstanceOf[Elem[NamedTuple[N, V], n.type]]

    /** The number of elements in this tuple. */
    inline def size: Size[NamedTuple[N, V]] = x.toTuple.size

    /** The first element value of this tuple. */
    inline def head: Head[NamedTuple[N, V]] = apply(0)

    /** The last element value of this tuple. */
    inline def last: Last[NamedTuple[N, V]] = apply(size - 1).asInstanceOf[Last[NamedTuple[N, V]]]

    /** The tuple consisting of all elements of this tuple except the last one. */
    inline def init: Init[NamedTuple[N, V]] =
      x.take(size - 1).asInstanceOf[Init[NamedTuple[N, V]]]

    /** The tuple consisting of all elements of this tuple except the first one. */
    inline def tail: Tail[NamedTuple[N, V]] = x.toTuple.drop(1)

    /** The tuple consisting of the first `n` elements of this tuple, or all
     *  elements if `n` exceeds `size`.
     *
     *  @param n the number of elements to take from this tuple
     */
    inline def take(n: Int): Take[NamedTuple[N, V], n.type] = x.toTuple.take(n)

    /** The tuple consisting of all elements of this tuple except the first `n` ones,
     *  or no elements if `n` exceeds `size`.
     *
     *  @param n the number of elements to drop from the beginning of this tuple
     */
    inline def drop(n: Int): Drop[NamedTuple[N, V], n.type] = x.toTuple.drop(n)

    /** The tuple `(x.take(n), x.drop(n))`.
     *
     *  @param n the index at which to split this tuple
     */
    inline def splitAt(n: Int): Split[NamedTuple[N, V], n.type] = x.toTuple.splitAt(n)

    /** The tuple consisting of all elements of this tuple followed by all elements
     *  of tuple `that`. The names of the two tuples must be disjoint.
     *
     *  @tparam N2 the tuple of name types of the other named tuple
     *  @tparam V2 the tuple of value types of the other named tuple
     *  @param that the named tuple to append to this one
     */
    inline def ++ [N2 <: Tuple, V2 <: Tuple](that: NamedTuple[N2, V2])(using Tuple.Disjoint[N, N2] =:= true)
      : Concat[NamedTuple[N, V], NamedTuple[N2, V2]]
      = x.toTuple ++ that.toTuple

    /** The named tuple consisting of all element values of this tuple mapped by
     *  the polymorphic mapping function `f`. The names of elements are preserved.
     *  If `x = (n1 = v1, ..., ni = vi)` then `x.map(f) = `(n1 = f(v1), ..., ni = f(vi))`.
     *
     *  @tparam F the type constructor applied to each element value type
     */
    inline def map[F[_]](f: [t] => t => F[t]): Map[NamedTuple[N, V], F] =
      x.toTuple.map[F](f)

    /** The named tuple consisting of all elements of this tuple in reverse. */
    inline def reverse: Reverse[NamedTuple[N, V]] = x.toTuple.reverse

    /** The named tuple consisting of all element values of this tuple zipped
     *  with corresponding element values in named tuple `that`.
     *  If the two tuples have different sizes,
     *  the extra elements of the larger tuple will be disregarded.
     *  The names of `x` and `that` at the same index must be the same.
     *  The result tuple keeps the same names as the operand tuples.
     *
     *  @tparam V2 the tuple of value types of the other named tuple
     *  @param that the named tuple to zip with this one
     */
    inline def zip[V2 <: Tuple](that: NamedTuple[N, V2]): Zip[NamedTuple[N, V], NamedTuple[N, V2]] =
      x.toTuple.zip(that.toTuple)

    /** A list consisting of all element values. */
    inline def toList: List[Tuple.Union[V]] = x.toTuple.toList

    /** An array consisting of all element values. */
    inline def toArray: Array[Object] = x.toTuple.toArray

    /** An immutable array consisting of all element values. */
    inline def toIArray: IArray[Object] = x.toTuple.toIArray

    /** An immutable map consisting of all element values preserving the order of fields.
     *  Keys are the names of the elements.
     */
    inline def toSeqMap: SeqMap[String, Tuple.Union[V]] =
      inline compiletime.constValueTuple[N].toList match
        case names: List[String] =>
          ListMap.from(names.iterator.zip(
            x.toTuple.productIterator.asInstanceOf[Iterator[Tuple.Union[V]]]
          ))
  end extension

  /** The names of a named tuple, represented as a tuple of literal string values. */
  type Names[X <: AnyNamedTuple] <: Tuple = X match
    case NamedTuple[n, ?] => n

  /** The value types of a named tuple represented as a regular tuple. */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[?, x] => x
