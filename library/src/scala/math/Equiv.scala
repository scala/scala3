/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

import scala.language.`2.13`
import java.util.Comparator
import scala.annotation.migration

/** A trait for representing equivalence relations.  It is important to
 *  distinguish between a type that can be compared for equality or
 *  equivalence and a representation of equivalence on some type. This
 *  trait is for representing the latter.
 *
 *  An [equivalence relation](https://en.wikipedia.org/wiki/Equivalence_relation)
 *  is a binary relation on a type. This relation is exposed as
 *  the `equiv` method of the `Equiv` trait.  The relation must be:
 *
 *    1. reflexive: `equiv(x, x) == true` for any x of type `T`.
 *    1. symmetric: `equiv(x, y) == equiv(y, x)` for any `x` and `y` of type `T`.
 *    1. transitive: if `equiv(x, y) == true` and `equiv(y, z) == true`, then
 *       `equiv(x, z) == true` for any `x`, `y`, and `z` of type `T`.
 *
 *  @tparam T the type of values being compared for equivalence
 */

trait Equiv[T] extends Any with Serializable {
  /** Returns `true` iff `x` is equivalent to `y`.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  def equiv(x: T, y: T): Boolean
}

/** A trait containing low-priority implicit `Equiv` instances.
 *
 *  These instances are inherited into `Equiv`'s implicit scope but are
 *  deprioritized via subclassing.
 */
trait LowPriorityEquiv {
  self: Equiv.type =>

  /** Use `Equiv.universal` explicitly instead. If you really want an implicit universal `Equiv` instance
   *  despite the potential problems, consider `implicit def universalEquiv[T]: Equiv[T] = universal[T]`.
   *
   *  @tparam T the type of values to compare
   *  @return an `Equiv[T]` that compares values using universal equality (`==`), as returned by `Equiv.universal`
   *
   *  @deprecated This implicit universal `Equiv` instance allows accidentally
   *  comparing instances of types for which equality isn't well-defined or implemented.
   *  (For example, it does not make sense to compare two `Function1` instances.)
   */
  @deprecated("Use explicit Equiv.universal instead. See Scaladoc entry for more information: " +
    "https://www.scala-lang.org/api/current/scala/math/Equiv$.html#universalEquiv[T]:scala.math.Equiv[T]",
    since = "2.13.0")
  implicit def universalEquiv[T]: Equiv[T] = universal[T]
}

object Equiv extends LowPriorityEquiv {
  /** Returns an `Equiv` instance that uses reference equality (`eq`) for any reference type `T`.
   *
   *  @tparam T the reference type of values to compare, a subtype of `AnyRef`
   *  @return an `Equiv[T]` that compares values using `eq`
   */
  def reference[T <: AnyRef]: Equiv[T] = { _ eq _ }
  /** Returns an `Equiv` instance that uses universal equality (`==`) for any type `T`.
   *
   *  @tparam T the type of values to compare
   *  @return an `Equiv[T]` that compares values using `==`
   */
  def universal[T]: Equiv[T] = { _ == _ }
  /** Returns an `Equiv` instance from a `Comparator`.
   *
   *  @tparam T the type of values to compare
   *  @param cmp the `Comparator` used to compare values
   *  @return an `Equiv[T]` that uses `cmp.compare(x, y) == 0` to determine equivalence
   */
  def fromComparator[T](cmp: Comparator[T]): Equiv[T] = {
    (x, y) => cmp.compare(x, y) == 0
  }
  /** Creates an `Equiv` instance from a comparison function.
   *
   *  @tparam T the type of values to compare
   *  @param cmp the function used to compare values
   *  @return an `Equiv[T]` that uses `cmp(x, y)` to determine equivalence
   */
  def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = {
    (x, y) => cmp(x, y)
  }
  /** Creates an `Equiv` instance that compares values of type `T` by first applying a function `f` to them.
   *
   *  @tparam T the type of values to compare
   *  @tparam S the type of values returned by `f`, for which an `Equiv` instance is available
   *  @param f the function applied to values before comparison
   *  @return an `Equiv[T]` that compares values by applying `f` and then using the `Equiv[S]` instance
   */
  def by[T, S: Equiv](f: T => S): Equiv[T] =
    ((x, y) => implicitly[Equiv[S]].equiv(f(x), f(y)))

  /** Returns the implicit `Equiv` instance for type `T`.
   *
   *  @tparam T the type for which an `Equiv` instance is requested
   *  @return the implicit `Equiv[T]` instance
   */
  @inline def apply[T: Equiv]: Equiv[T] = implicitly[Equiv[T]]

  /* copied from Ordering */

  private final val optionSeed   = 43
  private final val iterableSeed = 47

  private final class IterableEquiv[CC[X] <: Iterable[X], T](private val eqv: Equiv[T]) extends Equiv[CC[T]] {
    /** Returns `true` if the two iterables `x` and `y` are equivalent.
     *
     *  @param x the first iterable to compare
     *  @param y the second iterable to compare
     *  @return `true` if `x` and `y` have the same length and all corresponding elements are equivalent in iteration order, `false` otherwise
     */
    def equiv(x: CC[T], y: CC[T]): Boolean = {
      val xe = x.iterator
      val ye = y.iterator

      while (xe.hasNext && ye.hasNext) {
        if (!eqv.equiv(xe.next(), ye.next())) return false
      }

      xe.hasNext == ye.hasNext
    }

    /** Returns `true` if this `IterableEquiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is an `IterableEquiv` with the same underlying `Equiv` instance
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that  => true
      case that: IterableEquiv[?, ?]     => this.eqv == that.eqv
      case _                             => false
    }
    /** Returns a hash code for this `IterableEquiv`.
     *
     *  The hash code is computed as the hash code of the underlying `Equiv` instance multiplied by a seed value.
     */
    override def hashCode(): Int = eqv.## * iterableSeed
  }

  /** A trait containing additional implicit `Equiv` instances for collections.
   *
   *  These instances are not in the default scope to avoid potential divergence issues.
   */
  trait ExtraImplicits {
    /** Not in the standard scope due to the potential for divergence:
     *  For instance `implicitly[Equiv[Any]]` diverges in its presence.
     *
     *  @tparam CC the collection type constructor, a subtype of `Seq` (e.g., `List`, `Vector`)
     *  @tparam T the element type of the collection
     *  @param eqv the `Equiv` instance used to compare individual elements of type `T`
     *  @return an `Equiv` for sequences of type `CC[T]` that compares them element-by-element using `eqv`
     */
    implicit def seqEquiv[CC[X] <: scala.collection.Seq[X], T](implicit eqv: Equiv[T]): Equiv[CC[T]] =
      new IterableEquiv[CC, T](eqv)

    /**
     *  @tparam CC the collection type constructor, a subtype of `SortedSet`
     *  @tparam T the element type of the collection
     *  @param eqv the `Equiv` instance used to compare individual elements of type `T`
     *  @return an `Equiv` for sorted sets of type `CC[T]` that compares them element-by-element in iteration order using `eqv`
     */
    implicit def sortedSetEquiv[CC[X] <: scala.collection.SortedSet[X], T](implicit eqv: Equiv[T]): Equiv[CC[T]] =
      new IterableEquiv[CC, T](eqv)
  }

  /** An object containing implicits which are not in the default scope. */
  object Implicits extends ExtraImplicits { }

  implicit object Unit extends Equiv[Unit] {
    /** Returns `true` if the two `Unit` values are equivalent.
     *
     *  @param x the first `Unit` value to compare
     *  @param y the second `Unit` value to compare
     *  @return `true` since all `Unit` values are equivalent
     */
    def equiv(x: Unit, y: Unit): Boolean = true
  }

  implicit object Boolean extends Equiv[Boolean] {
    /** Returns `true` if the two `Boolean` values are equivalent.
     *
     *  @param x the first `Boolean` value to compare
     *  @param y the second `Boolean` value to compare
     */
    def equiv(x: Boolean, y: Boolean): Boolean = x == y
  }

  implicit object Byte extends Equiv[Byte] {
    /** Returns `true` if the two `Byte` values are equivalent.
     *
     *  @param x the first `Byte` value to compare
     *  @param y the second `Byte` value to compare
     */
    def equiv(x: Byte, y: Byte): Boolean = x == y
  }

  implicit object Char extends Equiv[Char] {
    /** Returns `true` if the two `Char` values are equivalent.
     *
     *  @param x the first `Char` value to compare
     *  @param y the second `Char` value to compare
     */
    def equiv(x: Char, y: Char): Boolean = x == y
  }

  implicit object Short extends Equiv[Short] {
    /** Returns `true` if the two `Short` values are equivalent.
     *
     *  @param x the first `Short` value to compare
     *  @param y the second `Short` value to compare
     */
    def equiv(x: Short, y: Short): Boolean = x == y
  }

  implicit object Int extends Equiv[Int] {
    /** Returns `true` if the two `Int` values are equivalent.
     *
     *  @param x the first `Int` value to compare
     *  @param y the second `Int` value to compare
     */
    def equiv(x: Int, y: Int): Boolean = x == y
  }

  implicit object Long extends Equiv[Long] {
    /** Returns `true` if the two `Long` values are equivalent.
     *
     *  @param x the first `Long` value to compare
     *  @param y the second `Long` value to compare
     */
    def equiv(x: Long, y: Long): Boolean = x == y
  }

  /** `Equiv`s for `Float`s.
   *
   *  @define floatEquiv Because the behaviour of `Float`s specified by IEEE is
   *                    not consistent with behaviors required of an equivalence
   *                    relation for `NaN` (it is not reflexive), there are two
   *                    equivalences defined for `Float`: `StrictEquiv`, which
   *                    is reflexive, and `IeeeEquiv`, which is consistent
   *                    with IEEE spec and floating point operations defined in
   *                    [[scala.math]].
   */
  object Float {
    /** An equivalence for `Float`s which is reflexive (treats all `NaN`s
     *  as equivalent), and treats `-0.0` and `0.0` as not equivalent; it
     *  behaves the same as [[java.lang.Float.compare]].
     *
     *  $floatEquiv
     *
     *  This equivalence may be preferable for collections.
     *
     *  @see [[IeeeEquiv]]
     */
    trait StrictEquiv extends Equiv[Float] {
      /** Returns `true` if the two `Float` values are equivalent.
       *
       *  This method uses `java.lang.Float.compare` to compare the values, which treats all `NaN` values as equivalent and distinguishes between `-0.0` and `0.0`.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` and `y` are equivalent according to `java.lang.Float.compare`, `false` otherwise
       */
      def equiv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) == 0
    }
    implicit object StrictEquiv extends StrictEquiv

    /** An equivalence for `Float`s which is consistent with IEEE specifications.
     *
     *  $floatEquiv
     *
     *  This equivalence may be preferable for numeric contexts.
     *
     *  @see [[StrictEquiv]]
     */
    trait IeeeEquiv extends Equiv[Float] {
      /** Returns `true` if the two `Float` values are equivalent.
       *
       *  This method uses `==` to compare the values, which is consistent with IEEE specifications but not reflexive for `NaN` values.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` and `y` are equal according to `==`, `false` otherwise
       */
      override def equiv(x: Float, y: Float): Boolean = x == y
    }
    implicit object IeeeEquiv extends IeeeEquiv
  }

  @migration(
    "  The default implicit equivalence for floats no longer conforms to\n" +
    "  to IEEE 754's behavior for -0.0F and NaN.\n" +
    "  Import `Equiv.Float.IeeeEquiv` to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Equiv$$Float$.html.", "2.13.2")
  implicit object DeprecatedFloatEquiv extends Float.StrictEquiv

  /** `Equiv`s for `Double`s.
   *
   *  @define doubleEquiv Because the behaviour of `Double`s specified by IEEE is
   *                     not consistent with behaviors required of an equivalence
   *                     relation for `NaN` (it is not reflexive), there are two
   *                     equivalences defined for `Double`: `StrictEquiv`, which
   *                     is reflexive, and `IeeeEquiv`, which is consistent
   *                     with IEEE spec and floating point operations defined in
   *                     [[scala.math]].
   */
  object Double {
    /** An equivalence for `Double`s which is reflexive (treats all `NaN`s
     *  as equivalent), and treats `-0.0` and `0.0` as not equivalent; it
     *  behaves the same as [[java.lang.Double.compare]].
     *
     *  $doubleEquiv
     *
     *  This equivalence may be preferable for collections.
     *
     *  @see [[IeeeEquiv]]
     */
    trait StrictEquiv extends Equiv[Double] {
      /** Returns `true` if the two `Double` values are equivalent.
       *
       *  This method uses `java.lang.Double.compare` to compare the values, which treats all `NaN` values as equivalent and distinguishes between `-0.0` and `0.0`.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` and `y` are equivalent according to `java.lang.Double.compare`, `false` otherwise
       */
      def equiv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) == 0
    }
    implicit object StrictEquiv extends StrictEquiv

    /** An equivalence for `Double`s which is consistent with IEEE specifications.
     *
     *  $doubleEquiv
     *
     *  This equivalence may be preferable for numeric contexts.
     *
     *  @see [[StrictEquiv]]
     */
    trait IeeeEquiv extends Equiv[Double] {
      /** Returns `true` if the two `Double` values are equivalent.
       *
       *  This method uses `==` to compare the values, which is consistent with IEEE specifications but not reflexive for `NaN` values.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` and `y` are equal according to `==`, `false` otherwise
       */
      def equiv(x: Double, y: Double): Boolean = x == y
    }
    implicit object IeeeEquiv extends IeeeEquiv
  }
  @migration(
    "  The default implicit equivalence for doubles no longer conforms to\n" +
    "  to IEEE 754's behavior for -0.0D and NaN.\n" +
    "  Import `Equiv.Double.IeeeEquiv` to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Equiv$$Double$.html.", "2.13.2")
  implicit object DeprecatedDoubleEquiv extends Double.StrictEquiv

  implicit object BigInt extends Equiv[BigInt] {
    /** Returns `true` if the two `BigInt` values are equivalent.
     *
     *  @param x the first `BigInt` value to compare
     *  @param y the second `BigInt` value to compare
     */
    def equiv(x: BigInt, y: BigInt): Boolean = x == y
  }

  implicit object BigDecimal extends Equiv[BigDecimal] {
    /** Returns `true` if the two `BigDecimal` values are equivalent.
     *
     *  @param x the first `BigDecimal` value to compare
     *  @param y the second `BigDecimal` value to compare
     */
    def equiv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  }

  implicit object String extends Equiv[String] {
    /** Returns `true` if the two `String` values are equivalent.
     *
     *  @param x the first `String` value to compare
     *  @param y the second `String` value to compare
     */
    def equiv(x: String, y: String): Boolean = x == y
  }

  implicit object Symbol extends Equiv[Symbol] {
    /** Returns `true` if the two `Symbol` values are equivalent.
     *
     *  @param x the first `Symbol` value to compare
     *  @param y the second `Symbol` value to compare
     */
    def equiv(x: Symbol, y: Symbol): Boolean = x == y
  }

  /** Returns an `Equiv` instance for `Option[T]` that uses the given `Equiv[T]` instance to compare the elements.
   *
   *  @tparam T the type of the elements in the `Option`
   *  @param eqv the `Equiv` instance used to compare the elements
   *  @return an `Equiv[Option[T]]` that compares `Option` values using `eqv`
   */
  implicit def Option[T](implicit eqv: Equiv[T]): Equiv[Option[T]] =
    new OptionEquiv[T](eqv)

  private final class OptionEquiv[T](private val eqv: Equiv[T]) extends Equiv[Option[T]] {
    /** Returns `true` if the two `Option` values are equivalent.
     *
     *  @param x the first `Option` value to compare
     *  @param y the second `Option` value to compare
     *  @return `true` if both are `None` or both are `Some` with equivalent elements
     */
    def equiv(x: Option[T], y: Option[T]): Boolean = (x, y) match {
      case (None, None)       => true
      case (Some(x), Some(y)) => eqv.equiv(x, y)
      case _                  => false
    }

    /** Returns `true` if this `OptionEquiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is an `OptionEquiv` with the same underlying `Equiv` instance
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: OptionEquiv[?]         => this.eqv == that.eqv
      case _                            => false
    }
    /** Returns a hash code for this `OptionEquiv`.
     *
     *  The hash code is computed as the hash code of the underlying `Equiv` instance multiplied by a seed value.
     */
    override def hashCode(): Int = eqv.## * optionSeed
  }

  /** Returns an `Equiv` instance for `Tuple2[T1, T2]` that uses the given `Equiv[T1]` and `Equiv[T2]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @return an `Equiv[(T1, T2)]` that compares tuples using `eqv1` and `eqv2`
   */
  implicit def Tuple2[T1, T2](implicit eqv1: Equiv[T1], eqv2: Equiv[T2]): Equiv[(T1, T2)] =
    new Tuple2Equiv(eqv1, eqv2)

  private final class Tuple2Equiv[T1, T2](private val eqv1: Equiv[T1],
                                                private val eqv2: Equiv[T2]) extends Equiv[(T1, T2)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if both elements are equivalent
     */
    def equiv(x: (T1, T2), y: (T1, T2)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2)

    /** Returns `true` if this `Tuple2Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple2Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple2Equiv[?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2
      case _ => false
    }
    /** Returns a hash code for this `Tuple2Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple3[T1, T2, T3]` that uses the given `Equiv[T1]`, `Equiv[T2]`, and `Equiv[T3]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @return an `Equiv[(T1, T2, T3)]` that compares tuples using `eqv1`, `eqv2`, and `eqv3`
   */
  implicit def Tuple3[T1, T2, T3](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3]) : Equiv[(T1, T2, T3)] =
    new Tuple3Equiv(eqv1, eqv2, eqv3)

  private final class Tuple3Equiv[T1, T2, T3](private val eqv1: Equiv[T1],
                                                    private val eqv2: Equiv[T2],
                                                    private val eqv3: Equiv[T3]) extends Equiv[(T1, T2, T3)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all three elements are equivalent
     */
    def equiv(x: (T1, T2, T3), y: (T1, T2, T3)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3)

    /** Returns `true` if this `Tuple3Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple3Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple3Equiv[?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3
      case _ => false
    }
    /** Returns a hash code for this `Tuple3Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple4[T1, T2, T3, T4]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, and `Equiv[T4]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @return an `Equiv[(T1, T2, T3, T4)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, and `eqv4`
   */
  implicit def Tuple4[T1, T2, T3, T4](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4]) : Equiv[(T1, T2, T3, T4)] =
    new Tuple4Equiv(eqv1, eqv2, eqv3, eqv4)

  private final class Tuple4Equiv[T1, T2, T3, T4](private val eqv1: Equiv[T1],
                                                        private val eqv2: Equiv[T2],
                                                        private val eqv3: Equiv[T3],
                                                        private val eqv4: Equiv[T4])
    extends Equiv[(T1, T2, T3, T4)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all four elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4), y: (T1, T2, T3, T4)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4)

    /** Returns `true` if this `Tuple4Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple4Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple4Equiv[?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4
      case _ => false
    }
    /** Returns a hash code for this `Tuple4Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple5[T1, T2, T3, T4, T5]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, `Equiv[T4]`, and `Equiv[T5]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @param eqv5 the `Equiv` instance used to compare the fifth elements
   *  @return an `Equiv[(T1, T2, T3, T4, T5)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, `eqv4`, and `eqv5`
   */
  implicit def Tuple5[T1, T2, T3, T4, T5](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5]): Equiv[(T1, T2, T3, T4, T5)] =
    new Tuple5Equiv(eqv1, eqv2, eqv3, eqv4, eqv5)

  private final class Tuple5Equiv[T1, T2, T3, T4, T5](private val eqv1: Equiv[T1],
                                                            private val eqv2: Equiv[T2],
                                                            private val eqv3: Equiv[T3],
                                                            private val eqv4: Equiv[T4],
                                                            private val eqv5: Equiv[T5])
    extends Equiv[(T1, T2, T3, T4, T5)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all five elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4, T5), y: (T1, T2, T3, T4, T5)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5)

    /** Returns `true` if this `Tuple5Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple5Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple5Equiv[?, ?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5
      case _ => false
    }
    /** Returns a hash code for this `Tuple5Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple6[T1, T2, T3, T4, T5, T6]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, `Equiv[T4]`, `Equiv[T5]`, and `Equiv[T6]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @param eqv5 the `Equiv` instance used to compare the fifth elements
   *  @param eqv6 the `Equiv` instance used to compare the sixth elements
   *  @return an `Equiv[(T1, T2, T3, T4, T5, T6)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, `eqv4`, `eqv5`, and `eqv6`
   */
  implicit def Tuple6[T1, T2, T3, T4, T5, T6](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6]): Equiv[(T1, T2, T3, T4, T5, T6)] =
    new Tuple6Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6)

  private final class Tuple6Equiv[T1, T2, T3, T4, T5, T6](private val eqv1: Equiv[T1],
                                                                private val eqv2: Equiv[T2],
                                                                private val eqv3: Equiv[T3],
                                                                private val eqv4: Equiv[T4],
                                                                private val eqv5: Equiv[T5],
                                                                private val eqv6: Equiv[T6])
    extends Equiv[(T1, T2, T3, T4, T5, T6)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all six elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4, T5, T6), y: (T1, T2, T3, T4, T5, T6)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6)

    /** Returns `true` if this `Tuple6Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple6Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple6Equiv[?, ?, ?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6
      case _ => false
    }
    /** Returns a hash code for this `Tuple6Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple7[T1, T2, T3, T4, T5, T6, T7]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, `Equiv[T4]`, `Equiv[T5]`, `Equiv[T6]`, and `Equiv[T7]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @tparam T7 the type of the seventh element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @param eqv5 the `Equiv` instance used to compare the fifth elements
   *  @param eqv6 the `Equiv` instance used to compare the sixth elements
   *  @param eqv7 the `Equiv` instance used to compare the seventh elements
   *  @return an `Equiv[(T1, T2, T3, T4, T5, T6, T7)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, `eqv4`, `eqv5`, `eqv6`, and `eqv7`
   */
  implicit def Tuple7[T1, T2, T3, T4, T5, T6, T7](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7]): Equiv[(T1, T2, T3, T4, T5, T6, T7)] =
    new Tuple7Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7)

  private final class Tuple7Equiv[T1, T2, T3, T4, T5, T6, T7](private val eqv1: Equiv[T1],
                                                                    private val eqv2: Equiv[T2],
                                                                    private val eqv3: Equiv[T3],
                                                                    private val eqv4: Equiv[T4],
                                                                    private val eqv5: Equiv[T5],
                                                                    private val eqv6: Equiv[T6],
                                                                    private val eqv7: Equiv[T7])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all seven elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7), y: (T1, T2, T3, T4, T5, T6, T7)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7)

    /** Returns `true` if this `Tuple7Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple7Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple7Equiv[?, ?, ?, ?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7
      case _ => false
    }
    /** Returns a hash code for this `Tuple7Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, `Equiv[T4]`, `Equiv[T5]`, `Equiv[T6]`, `Equiv[T7]`, and `Equiv[T8]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @tparam T7 the type of the seventh element in the tuple
   *  @tparam T8 the type of the eighth element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @param eqv5 the `Equiv` instance used to compare the fifth elements
   *  @param eqv6 the `Equiv` instance used to compare the sixth elements
   *  @param eqv7 the `Equiv` instance used to compare the seventh elements
   *  @param eqv8 the `Equiv` instance used to compare the eighth elements
   *  @return an `Equiv[(T1, T2, T3, T4, T5, T6, T7, T8)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, `eqv4`, `eqv5`, `eqv6`, `eqv7`, and `eqv8`
   */
  implicit def Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7], eqv8: Equiv[T8]): Equiv[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    new Tuple8Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8)

  private final class Tuple8Equiv[T1, T2, T3, T4, T5, T6, T7, T8](private val eqv1: Equiv[T1],
                                                                        private val eqv2: Equiv[T2],
                                                                        private val eqv3: Equiv[T3],
                                                                        private val eqv4: Equiv[T4],
                                                                        private val eqv5: Equiv[T5],
                                                                        private val eqv6: Equiv[T6],
                                                                        private val eqv7: Equiv[T7],
                                                                        private val eqv8: Equiv[T8])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all eight elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7, T8), y: (T1, T2, T3, T4, T5, T6, T7, T8)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7) &&
      eqv8.equiv(x._8, y._8)

    /** Returns `true` if this `Tuple8Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple8Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple8Equiv[?, ?, ?, ?, ?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7 &&
        this.eqv8 == that.eqv8
      case _ => false
    }
    /** Returns a hash code for this `Tuple8Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8).hashCode()
  }

  /** Returns an `Equiv` instance for `Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]` that uses the given `Equiv[T1]`, `Equiv[T2]`, `Equiv[T3]`, `Equiv[T4]`, `Equiv[T5]`, `Equiv[T6]`, `Equiv[T7]`, `Equiv[T8]`, and `Equiv[T9]` instances to compare the elements.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @tparam T7 the type of the seventh element in the tuple
   *  @tparam T8 the type of the eighth element in the tuple
   *  @tparam T9 the type of the ninth element in the tuple
   *  @param eqv1 the `Equiv` instance used to compare the first elements
   *  @param eqv2 the `Equiv` instance used to compare the second elements
   *  @param eqv3 the `Equiv` instance used to compare the third elements
   *  @param eqv4 the `Equiv` instance used to compare the fourth elements
   *  @param eqv5 the `Equiv` instance used to compare the fifth elements
   *  @param eqv6 the `Equiv` instance used to compare the sixth elements
   *  @param eqv7 the `Equiv` instance used to compare the seventh elements
   *  @param eqv8 the `Equiv` instance used to compare the eighth elements
   *  @param eqv9 the `Equiv` instance used to compare the ninth elements
   *  @return an `Equiv[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]` that compares tuples using `eqv1`, `eqv2`, `eqv3`, `eqv4`, `eqv5`, `eqv6`, `eqv7`, `eqv8`, and `eqv9`
   */
  implicit def Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7], eqv8 : Equiv[T8], eqv9: Equiv[T9]): Equiv[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new Tuple9Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8, eqv9)

  private final class Tuple9Equiv[T1, T2, T3, T4, T5, T6, T7, T8, T9](private val eqv1: Equiv[T1],
                                                                            private val eqv2: Equiv[T2],
                                                                            private val eqv3: Equiv[T3],
                                                                            private val eqv4: Equiv[T4],
                                                                            private val eqv5: Equiv[T5],
                                                                            private val eqv6: Equiv[T6],
                                                                            private val eqv7: Equiv[T7],
                                                                            private val eqv8: Equiv[T8],
                                                                            private val eqv9: Equiv[T9])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    /** Returns `true` if the two tuples are equivalent.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return `true` if all nine elements are equivalent
     */
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7, T8, T9), y: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7) &&
      eqv8.equiv(x._8, y._8) &&
      eqv9.equiv(x._9, y._9)

    /** Returns `true` if this `Tuple9Equiv` is equal to `obj`.
     *
     *  @param obj the object to compare with
     *  @return `true` if `obj` is a `Tuple9Equiv` with the same underlying `Equiv` instances
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple9Equiv[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7 &&
        this.eqv8 == that.eqv8 &&
        this.eqv9 == that.eqv9
      case _ => false
    }
    /** Returns a hash code for this `Tuple9Equiv`.
     *
     *  The hash code is computed as the hash code of the tuple of the underlying `Equiv` instances.
     */
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8, eqv9).hashCode()
  }

}
