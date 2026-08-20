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

import scala.language.implicitConversions
import scala.annotation.migration
import scala.annotation.unchecked.uncheckedOverride

/** Ordering is a trait whose instances each represent a strategy for sorting
 *  instances of a type.
 *
 *  Ordering's companion object defines many implicit objects to deal with
 *  subtypes of [[AnyVal]] (e.g. `Int`, `Double`), `String`, and others.
 *
 *  To sort instances by one or more member variables, you can take advantage
 *  of these built-in orderings using [[Ordering.by]] and [[Ordering.on]]:
 *
 *  ```scala sc:compile
 *  import scala.util.Sorting
 *  val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))
 *
 *  // sort by 2nd element
 *  {
 *    given Ordering[(String, Int, Int)] = Ordering.by[(String, Int, Int), Int](_._2)
 *    Sorting.quickSort(pairs)
 *  }
 *
 *  // sort by the 3rd element, then 1st
 *  {
 *    given Ordering[(String, Int, Int)] = Ordering.by[(String, Int, Int), (Int, String)](x => (x._3, x._1))
 *    Sorting.quickSort(pairs)
 *  }
 *  ```
 *
 *  An `Ordering[T]` is implemented by specifying the [[compare]] method,
 *  `compare(a: T, b: T): Int`, which decides how to order two instances
 *  `a` and `b`. Instances of `Ordering[T]` can be used by things like
 *  `scala.util.Sorting` to sort collections like `Array[T]`.
 *
 *  For example:
 *
 *  ```scala sc:compile
 *  import scala.util.Sorting
 *
 *  case class Person(name:String, age:Int)
 *  val people = Array(Person("bob", 30), Person("ann", 32), Person("carl", 19))
 *
 *  // sort by age
 *  object AgeOrdering extends Ordering[Person] {
 *   def compare(a:Person, b:Person) = a.age.compare(b.age)
 *  }
 *  given Ordering[Person] = AgeOrdering
 *  Sorting.quickSort(people)
 *  ```
 *
 *  This trait and [[scala.math.Ordered]] both provide this same functionality, but
 *  in different ways. A type `T` can be given a single way to order itself by
 *  extending `Ordered`. Using `Ordering`, this same type may be sorted in many
 *  other ways. `Ordered` and `Ordering` both provide implicits allowing them to be
 *  used interchangeably.
 *
 *  You can `import scala.math.Ordering.Implicits._` to gain access to other
 *  implicit orderings.
 *
 *  @see [[scala.math.Ordered]], [[scala.util.Sorting]], [[scala.math.Ordering.Implicits]]
 *
 *  @tparam T the type of objects that this ordering can compare
 */
trait Ordering[T] extends Comparator[T] with PartialOrdering[T] with Serializable {
  outer =>

  /** Returns whether a comparison between `x` and `y` is defined, and if so
   *  the result of `compare(x, y)`.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   *  @return `Some` containing the result of `compare(x, y)`, since `Ordering` always defines a comparison
   */
  def tryCompare(x: T, y: T): Some[Int] = Some(compare(x, y))

 /** Returns an integer whose sign communicates how x compares to y.
  *
  *  The result sign has the following meaning:
  *
  *  - negative if x < y
  *  - positive if x > y
  *  - zero otherwise (if x == y)
  *
  *  @param x the first value to compare
  *  @param y the second value to compare
  *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
  */
  def compare(x: T, y: T): Int

  /** Returns true if `x` <= `y` in the ordering.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  override def lteq(x: T, y: T): Boolean = compare(x, y) <= 0

  /** Returns true if `x` >= `y` in the ordering.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  override def gteq(x: T, y: T): Boolean = compare(x, y) >= 0

  /** Returns true if `x` < `y` in the ordering.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  override def lt(x: T, y: T): Boolean = compare(x, y) < 0

  /** Returns true if `x` > `y` in the ordering.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  override def gt(x: T, y: T): Boolean = compare(x, y) > 0

  /** Returns true if `x` == `y` in the ordering.
   *
   *  @param x the first value to compare
   *  @param y the second value to compare
   */
  override def equiv(x: T, y: T): Boolean = compare(x, y) == 0

  /** Returns `x` if `x` >= `y`, otherwise `y`.
   *
   *  @tparam U a subtype of `T`, used to preserve the specific type in the return value
   *  @param x the first candidate value
   *  @param y the second candidate value
   */
  @uncheckedOverride def max[U <: T](x: U, y: U): U = if (gteq(x, y)) x else y

  /** Returns `x` if `x` <= `y`, otherwise `y`.
   *
   *  @tparam U a subtype of `T`, used to preserve the specific type in the return value
   *  @param x the first candidate value
   *  @param y the second candidate value
   */
  @uncheckedOverride def min[U <: T](x: U, y: U): U = if (lteq(x, y)) x else y

  /** Returns the opposite ordering of this one.
   *
   *  Implementations overriding this method MUST override [[isReverseOf]]
   *  as well if they change the behavior at all (for example, caching does
   *  not require overriding it).
   *
   *  @return an `Ordering[T]` that compares elements in the reverse order of this ordering
   */
  override def reverse: Ordering[T] = new Ordering.Reverse[T](this)

  /** Returns whether or not the other ordering is the opposite
   *  ordering of this one.
   *
   *  Equivalent to `other == this.reverse`.
   *
   *  Implementations should only override this method if they are overriding
   *  [[reverse]] as well.
   *
   *  @param other the ordering to check
   *  @return `true` if `other` is the reverse of this ordering, `false` otherwise
   */
  def isReverseOf(other: Ordering[?]): Boolean = other match {
    case that: Ordering.Reverse[?] => that.outer == this
    case _ => false
  }

  /** Given f, a function from U into T, creates an Ordering[U] whose compare
   *  function is equivalent to:
   *
  *  ```scala sc:compile
  *  def compare[U, T: Ordering](x: U, y: U, f: U => T) = Ordering[T].compare(f(x), f(y))
   *  ```
   *
   *  @tparam U the type of the values to be ordered
   *  @param f the function to extract a `T` value from a `U` value
   *  @return an `Ordering[U]` that orders values by applying `f` and comparing the results using this ordering
   */
  def on[U](f: U => T): Ordering[U] = new Ordering[U] {
    def compare(x: U, y: U) = outer.compare(f(x), f(y))
  }

  /** Creates an Ordering[T] whose compare function returns the
   *  result of this Ordering's compare function, if it is non-zero,
   *  or else the result of `other`s compare function.
   *
   *  @example
  *  ```scala sc:compile
   *  case class Pair(a: Int, b: Int)
   *
   *  val pairOrdering = Ordering.by[Pair, Int](_.a)
   *                            .orElse(Ordering.by[Pair, Int](_.b))
   *  ```
   *
   *  @param other an Ordering to use if this Ordering returns zero
   *  @return an `Ordering[T]` that uses this ordering first, falling back to `other` when values are equal
   */
  def orElse(other: Ordering[T]): Ordering[T] = (x, y) => {
    val res1 = outer.compare(x, y)
    if (res1 != 0) res1 else other.compare(x, y)
  }

  /** Given f, a function from T into S, creates an Ordering[T] whose compare
   *  function returns the result of this Ordering's compare function,
   *  if it is non-zero, or else a result equivalent to:
   *
  *  ```scala sc:compile
  *  def compare[T, S: Ordering](x: T, y: T, f: T => S) = Ordering[S].compare(f(x), f(y))
   *  ```
   *
   *  This function is equivalent to passing the result of `Ordering.by(f)`
   *  to `orElse`.
   *
   *  @example
  *  ```scala sc:compile
   *  case class Pair(a: Int, b: Int)
   *
   *  val pairOrdering = Ordering.by[Pair, Int](_.a)
   *                            .orElseBy[Int](_.b)
   *  ```
   *
   *  @tparam S the type returned by the extraction function `f`
   *  @param f the function to extract a comparison key from a `T` value
   *  @param ord the implicit ordering for the extracted key type `S`
   *  @return an `Ordering[T]` that uses this ordering first, falling back to comparing by `f` when values are equal
   */
  def orElseBy[S](f: T => S)(implicit ord: Ordering[S]): Ordering[T] = (x, y) => {
    val res1 = outer.compare(x, y)
    if (res1 != 0) res1 else ord.compare(f(x), f(y))
  }

  /** This inner class defines comparison operators available for `T`.
   *
   *  It can't extend `AnyVal` because it is not a top-level class
   *  or a member of a statically accessible object.
   *
   *  @param lhs the left-hand side value for infix comparison operations
   */
  class OrderingOps(lhs: T) {
    /** Returns true if `lhs` is less than `rhs` in this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return `true` if `lhs` < `rhs`, `false` otherwise
     */
    def <(rhs: T): Boolean = lt(lhs, rhs)
    /** Returns true if `lhs` is less than or equal to `rhs` in this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return `true` if `lhs` <= `rhs`, `false` otherwise
     */
    def <=(rhs: T): Boolean = lteq(lhs, rhs)
    /** Returns true if `lhs` is greater than `rhs` in this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return `true` if `lhs` > `rhs`, `false` otherwise
     */
    def >(rhs: T): Boolean = gt(lhs, rhs)
    /** Returns true if `lhs` is greater than or equal to `rhs` in this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return `true` if `lhs` >= `rhs`, `false` otherwise
     */
    def >=(rhs: T): Boolean = gteq(lhs, rhs)
    /** Returns true if `lhs` is equivalent to `rhs` in this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return `true` if `lhs` == `rhs`, `false` otherwise
     */
    def equiv(rhs: T): Boolean = Ordering.this.equiv(lhs, rhs)
    /** Returns the greater of `lhs` and `rhs` according to this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return the maximum of `lhs` and `rhs`
     */
    def max(rhs: T): T = Ordering.this.max(lhs, rhs)
    /** Returns the lesser of `lhs` and `rhs` according to this ordering.
     *
     *  @param rhs the right-hand side value to compare with `lhs`
     *  @return the minimum of `lhs` and `rhs`
     */
    def min(rhs: T): T = Ordering.this.min(lhs, rhs)
  }

  /** This implicit method augments `T` with the comparison operators defined
   *  in `scala.math.Ordering.Ops`.
   *
   *  @param lhs the value to enrich with ordering operators
   *  @return an `OrderingOps` wrapping `lhs` and providing infix comparison operators
   */
  implicit def mkOrderingOps(lhs: T): OrderingOps = new OrderingOps(lhs)
}

/** Provides implicit orderings for types that don't have higher-priority implicit orderings available.
 *
 *  This trait contains lower-priority implicit orderings that are only used when no other
 *  implicit ordering is available in scope. It helps prevent ambiguous implicit conversions
 *  while still providing default ordering behavior.
 */
trait LowPriorityOrderingImplicits {

  type AsComparable[A] = A => Comparable[? >: A]

  /** This would conflict with all the nice implicit Orderings
   *  available, but thanks to the magic of prioritized implicits
   *  via subclassing we can make `Ordered[A] => Ordering[A]` only
   *  turn up if nothing else works.  Since `Ordered[A]` extends
   *  `Comparable[A]` anyway, we can throw in some Java interop too.
   *
   *  @tparam A the type to be ordered, which must be convertible to `Comparable`
   *  @param asComparable the implicit conversion from `A` to `Comparable[? >: A]`
   *  @return an `Ordering[A]` that compares values by delegating to their `Comparable.compareTo`
   */
  implicit def ordered[A](implicit asComparable: AsComparable[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = asComparable(x).compareTo(y)
  }

  /** Converts a Java `Comparator` to a Scala `Ordering`.
   *
   *  @tparam A the type of elements to be compared
   *  @param cmp the Java `Comparator` to convert
   *  @return an `Ordering[A]` that delegates to the provided `Comparator`
   */
  implicit def comparatorToOrdering[A](implicit cmp: Comparator[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A) = cmp.compare(x, y)
  }
}

/** This is the companion object for the [[scala.math.Ordering]] trait.
 *
 *  It contains many implicit orderings as well as well as methods to construct
 *  new orderings.
 */
object Ordering extends LowPriorityOrderingImplicits {
  private final val reverseSeed  = 41
  private final val optionSeed   = 43
  private final val iterableSeed = 47

  /** Retrieves the implicit `Ordering` for type `T`.
   *
   *  @tparam T the type for which to retrieve the ordering
   *  @param ord the implicit `Ordering[T]` instance
   */
  @inline def apply[T](implicit ord: Ordering[T]) = ord

  /** An ordering which caches the value of its reverse.
   *
   *  @tparam T the type of objects that this ordering can compare
   */
  sealed trait CachedReverse[T] extends Ordering[T] {
    private val _reverse = super.reverse
    /** Returns the cached reverse ordering of this ordering.
     */
    override final def reverse: Ordering[T] = _reverse
    /** Returns whether the given ordering is the cached reverse of this ordering.
     *
     *  @param other the ordering to check
     *  @return `true` if `other` is the cached reverse of this ordering, `false` otherwise
     */
    override final def isReverseOf(other: Ordering[?]): Boolean = other eq _reverse
  }

  /** A reverse ordering.
   *
   *  @tparam T the type of objects that this ordering can compare
   *  @param outer the original ordering to be reversed
   */
  private final class Reverse[T](private[Ordering] val outer: Ordering[T]) extends Ordering[T] {
    /** Returns the original ordering that this ordering reverses.
     */
    override def reverse: Ordering[T]                   = outer
    /** Returns whether the given ordering is the original ordering that this ordering reverses.
     *
     *  @param other the ordering to check
     *  @return `true` if `other` is the original ordering that this ordering reverses, `false` otherwise
     */
    override def isReverseOf(other: Ordering[?]): Boolean = other == outer

    /** Compares two values in reverse order.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is greater than, equal to, or less than `y`
     */
    def compare(x: T, y: T): Int            = outer.compare(y, x)
    /** Returns true if `x` is greater than or equal to `y` in the original ordering.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `true` if `x` >= `y` in the original ordering, `false` otherwise
     */
    override def lteq(x: T, y: T): Boolean  = outer.lteq(y, x)
    /** Returns true if `x` is less than or equal to `y` in the original ordering.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `true` if `x` <= `y` in the original ordering, `false` otherwise
     */
    override def gteq(x: T, y: T): Boolean  = outer.gteq(y, x)
    /** Returns true if `x` is greater than `y` in the original ordering.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `true` if `x` > `y` in the original ordering, `false` otherwise
     */
    override def lt(x: T, y: T): Boolean    = outer.lt(y, x)
    /** Returns true if `x` is less than `y` in the original ordering.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `true` if `x` < `y` in the original ordering, `false` otherwise
     */
    override def gt(x: T, y: T): Boolean    = outer.gt(y, x)
    /** Returns true if `x` is equivalent to `y` in the original ordering.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `true` if `x` == `y` in the original ordering, `false` otherwise
     */
    override def equiv(x: T, y: T): Boolean = outer.equiv(y, x)
    /** Returns the minimum of `x` and `y` in the original ordering.
     *
     *  @tparam U a subtype of `T`, used to preserve the specific type in the return value
     *  @param x the first candidate value
     *  @param y the second candidate value
     *  @return the minimum of `x` and `y` in the original ordering
     */
    override def max[U <: T](x: U, y: U): U = outer.min(x, y)
    /** Returns the maximum of `x` and `y` in the original ordering.
     *
     *  @tparam U a subtype of `T`, used to preserve the specific type in the return value
     *  @param x the first candidate value
     *  @param y the second candidate value
     *  @return the maximum of `x` and `y` in the original ordering
     */
    override def min[U <: T](x: U, y: U): U = outer.max(x, y)

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Reverse[?]             => this.outer == that.outer
      case _                            => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = outer.hashCode() * reverseSeed
  }

  @SerialVersionUID(-2996748994664583574L)
  private final class IterableOrdering[CC[X] <: Iterable[X], T](private val ord: Ordering[T]) extends Ordering[CC[T]] {
    /** Compares two iterables lexicographically using the element ordering.
     *
     *  @param x the first iterable to compare
     *  @param y the second iterable to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: CC[T], y: CC[T]): Int = {
      val xe = x.iterator
      val ye = y.iterator

      while (xe.hasNext && ye.hasNext) {
        val res = ord.compare(xe.next(), ye.next())
        if (res != 0) return res
      }

      Boolean.compare(xe.hasNext, ye.hasNext)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that  => true
      case that: IterableOrdering[?, ?]  => this.ord == that.ord
      case _                             => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = ord.hashCode() * iterableSeed
  }

  /** Provides additional implicit orderings that are not in the default scope.
   */
  trait ExtraImplicits {
    /** Not in the standard scope due to the potential for divergence:
     *  For instance `implicitly[Ordering[Any]]` diverges in its presence.
     *
     *  @tparam CC the higher-kinded type constructor for the sequence type, bounded by `scala.collection.Seq`
     *  @tparam T the element type of the sequences being compared
     *  @param ord the implicit `Ordering` used to compare elements of type `T`
     *  @return an `Ordering[CC[T]]` that compares sequences lexicographically using `ord`
     */
    implicit def seqOrdering[CC[X] <: scala.collection.Seq[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] =
      new IterableOrdering[CC, T](ord)

    /** Creates an ordering for sorted sets using the element ordering.
     *
     *  @tparam CC the higher-kinded type constructor for the sorted set type, bounded by `scala.collection.SortedSet`
     *  @tparam T the element type of the sorted sets being compared
     *  @param ord the implicit `Ordering` used to compare elements of type `T`
     *  @return an `Ordering[CC[T]]` that compares sorted sets lexicographically using `ord`
     */
    implicit def sortedSetOrdering[CC[X] <: scala.collection.SortedSet[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] =
      new IterableOrdering[CC, T](ord)

    /** This implicit creates a conversion from any value for which an
     *  implicit `Ordering` exists to the class which creates infix operations.
     *  With it imported, you can write methods as follows:
     *
    *  ```scala sc:compile
    *  import scala.math.Ordering.Implicits.*
    *  def lessThan[T: Ordering](x: T, y: T) = x < y
     *  ```
     *
     *  @tparam T the type of the value being compared
     *  @param x the value to enrich with infix ordering operators
     *  @param ord the implicit `Ordering` instance for type `T`
     *  @return an `OrderingOps` instance providing infix comparison operators
     */
    implicit def infixOrderingOps[T](x: T)(implicit ord: Ordering[T]): Ordering[T]#OrderingOps = new ord.OrderingOps(x)
  }

  /** An object containing implicits which are not in the default scope. */
  object Implicits extends ExtraImplicits { }

  /** Constructs an Ordering[T] given a function `lt`.
   *
   *  @tparam T the type of objects to be ordered
   *  @param cmp a function that returns `true` if the first argument is less than the second
   *  @return an `Ordering[T]` whose comparison is derived from `cmp`
   */
  def fromLessThan[T](cmp: (T, T) => Boolean): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = if (cmp(x, y)) -1 else if (cmp(y, x)) 1 else 0
    // overrides to avoid multiple comparisons
    override def lt(x: T, y: T): Boolean = cmp(x, y)
    override def gt(x: T, y: T): Boolean = cmp(y, x)
    override def gteq(x: T, y: T): Boolean = !cmp(x, y)
    override def lteq(x: T, y: T): Boolean = !cmp(y, x)
  }

  /** Given f, a function from T into S, creates an Ordering[T] whose compare
   *  function is equivalent to:
   *
  *  ```scala sc:compile
  *  def compare[T, S: Ordering](x: T, y: T, f: T => S) = Ordering[S].compare(f(x), f(y))
   *  ```
   *
   *  This function is an analogue to Ordering.on where the Ordering[S]
   *  parameter is passed implicitly.
   *
   *  @tparam T the type of objects to be ordered
   *  @tparam S the type of the sort key extracted by `f`
   *  @param f the function to extract a comparison key of type `S` from a value of type `T`
   *  @param ord the implicit ordering for the extracted key type `S`
   *  @return an `Ordering[T]` that orders values by applying `f` and comparing the results
   */
  def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = ord.compare(f(x), f(y))
    override def lt(x: T, y: T): Boolean = ord.lt(f(x), f(y))
    override def gt(x: T, y: T): Boolean = ord.gt(f(x), f(y))
    override def gteq(x: T, y: T): Boolean = ord.gteq(f(x), f(y))
    override def lteq(x: T, y: T): Boolean = ord.lteq(f(x), f(y))
  }

  /** An ordering for `Unit` values.
   */
  trait UnitOrdering extends Ordering[Unit] {
    /** Compares two `Unit` values.
     *
     *  @param x the first `Unit` value to compare
     *  @param y the second `Unit` value to compare
     */
    def compare(x: Unit, y: Unit) = 0
  }
  @SerialVersionUID(4089257611611206746L)
  implicit object Unit extends UnitOrdering

  /** An ordering for `Boolean` values.
   */
  trait BooleanOrdering extends Ordering[Boolean] {
    /** Compares two `Boolean` values.
     *
     *  @param x the first `Boolean` value to compare
     *  @param y the second `Boolean` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Boolean, y: Boolean): Int = java.lang.Boolean.compare(x, y)
  }
  @SerialVersionUID(-94703182178890445L)
  implicit object Boolean extends BooleanOrdering

  /** An ordering for `Byte` values.
   */
  trait ByteOrdering extends Ordering[Byte] {
    /** Compares two `Byte` values.
     *
     *  @param x the first `Byte` value to compare
     *  @param y the second `Byte` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Byte, y: Byte): Int = java.lang.Byte.compare(x, y)
  }
  @SerialVersionUID(-2268545360148786406L)
  implicit object Byte extends ByteOrdering

  /** An ordering for `Char` values.
   */
  trait CharOrdering extends Ordering[Char] {
    /** Compares two `Char` values.
     *
     *  @param x the first `Char` value to compare
     *  @param y the second `Char` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Char, y: Char): Int = java.lang.Character.compare(x, y)
  }
  @SerialVersionUID(2588141633104296698L)
  implicit object Char extends CharOrdering

  /** An ordering for `Short` values.
   */
  trait ShortOrdering extends Ordering[Short] {
    /** Compares two `Short` values.
     *
     *  @param x the first `Short` value to compare
     *  @param y the second `Short` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Short, y: Short): Int = java.lang.Short.compare(x, y)
  }
  @SerialVersionUID(4919657051864630912L)
  implicit object Short extends ShortOrdering

  /** An ordering for `Int` values.
   */
  trait IntOrdering extends Ordering[Int] {
    /** Compares two `Int` values.
     *
     *  @param x the first `Int` value to compare
     *  @param y the second `Int` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Int, y: Int): Int = java.lang.Integer.compare(x, y)
  }
  @SerialVersionUID(-8412871093094815037L)
  implicit object Int extends IntOrdering with CachedReverse[Int]

  /** An ordering for `Long` values.
   */
  trait LongOrdering extends Ordering[Long] {
    /** Compares two `Long` values.
     *
     *  @param x the first `Long` value to compare
     *  @param y the second `Long` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Long, y: Long): Int = java.lang.Long.compare(x, y)
  }
  @SerialVersionUID(-5231423581640563981L)
  implicit object Long extends LongOrdering

  /** `Ordering`s for `Float`s.
   *
   *  The default extends `Ordering.Float.TotalOrdering`.
   *
   *  `Ordering.Float.TotalOrdering` uses the `java.lang.Float.compare` semantics for all operations.
   *  Scala also provides the `Ordering.Float.IeeeOrdering` semantics. Which uses the IEEE 754 semantics
   *  for float ordering.
   *
   *  Historically: `IeeeOrdering` was used in Scala from 2.10.x through 2.12.x. This changed in 2.13.0
   *  to `TotalOrdering`.
   *
   *  Prior to Scala 2.10.0, the `Ordering` instance used semantics
   *  consistent with `java.lang.Float.compare`.
   *
   *  Scala 2.10.0 changed the implementation of `lt`, `equiv`, `min`, etc., to be
   *  IEEE 754 compliant, while keeping the `compare` method NOT compliant,
   *  creating an internally inconsistent instance. IEEE 754 specifies that
   *  `0.0F == -0.0F`. In addition, it requires all comparisons with `Float.NaN` return
   *  `false` thus `0.0F < Float.NaN`, `0.0F > Float.NaN`, and
   *  `Float.NaN == Float.NaN` all yield `false`, analogous `None` in `flatMap`.
   *
   *
  *  ```scala sc:compile
   *  List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).sorted      // List(-Infinity, 0.0, 1.0, NaN)
   *  List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).min         // -Infinity
   *  implicitly[Ordering[Float]].lt(0.0F, 0.0F / 0.0F)       // true
   *  {
   *    import Ordering.Float.IeeeOrdering
   *    List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).sorted    // List(-Infinity, 0.0, 1.0, NaN)
   *    List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).min       // NaN
   *    implicitly[Ordering[Float]].lt(0.0F, 0.0F / 0.0F)     // false
   *  }
   *  ```
   *
   *  @define floatOrdering Because the behavior of `Float`s specified by IEEE is
   *                       not consistent with a total ordering when dealing with
   *                       `NaN`, there are two orderings defined for `Float`:
   *                       `TotalOrdering`, which is consistent with a total
   *                       ordering, and `IeeeOrdering`, which is consistent
   *                       as much as possible with IEEE spec and floating point
   *                       operations defined in [[scala.math]].
   */
  object Float {
    /** An ordering for `Float`s which is a fully consistent total ordering,
     *  and treats `NaN` as larger than all other `Float` values; it behaves
     *  the same as [[java.lang.Float.compare]].
     *
     *  $floatOrdering
     *
     *  This ordering may be preferable for sorting collections.
     *
     *  @see [[IeeeOrdering]]
     */
    trait TotalOrdering extends Ordering[Float] {
      /** Compares two `Float` values using the total ordering.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       */
      def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
    }
    @SerialVersionUID(2951539161283192433L)
    implicit object TotalOrdering extends TotalOrdering

    /** An ordering for `Float`s which is consistent with IEEE specifications
     *  whenever possible.
     *
     *   - `lt`, `lteq`, `equiv`, `gteq` and `gt` are consistent with primitive
     *  comparison operations for `Float`s, and return `false` when called with
     *  `NaN`.
     *   - `min` and `max` are consistent with `math.min` and `math.max`, and
     *  return `NaN` when called with `NaN` as either argument.
     *   - `compare` behaves the same as [[java.lang.Float.compare]].
     *
     *  $floatOrdering
     *
     *  This ordering may be preferable for numeric contexts.
     *
     *  @see [[TotalOrdering]]
     */
    trait IeeeOrdering extends Ordering[Float] {
      /** Compares two `Float` values using the total ordering.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       */
      def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)

      /** Returns true if `x` is less than or equal to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` <= `y`, `false` otherwise
       */
      override def lteq(x: Float, y: Float): Boolean = x <= y
      /** Returns true if `x` is greater than or equal to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` >= `y`, `false` otherwise
       */
      override def gteq(x: Float, y: Float): Boolean = x >= y
      /** Returns true if `x` is less than `y` using IEEE 754 semantics.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` < `y`, `false` otherwise
       */
      override def lt(x: Float, y: Float): Boolean = x < y
      /** Returns true if `x` is greater than `y` using IEEE 754 semantics.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` > `y`, `false` otherwise
       */
      override def gt(x: Float, y: Float): Boolean = x > y
      /** Returns true if `x` is equivalent to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Float` value to compare
       *  @param y the second `Float` value to compare
       *  @return `true` if `x` == `y`, `false` otherwise
       */
      override def equiv(x: Float, y: Float): Boolean = x == y
      /** Returns the maximum of `x` and `y` using IEEE 754 semantics.
       *
       *  @tparam U a subtype of `Float`, used to preserve the specific type in the return value
       *  @param x the first candidate value
       *  @param y the second candidate value
       *  @return the maximum of `x` and `y`
       */
      override def max[U <: Float](x: U, y: U): U = math.max(x, y).asInstanceOf[U]
      /** Returns the minimum of `x` and `y` using IEEE 754 semantics.
       *
       *  @tparam U a subtype of `Float`, used to preserve the specific type in the return value
       *  @param x the first candidate value
       *  @param y the second candidate value
       *  @return the minimum of `x` and `y`
       */
      override def min[U <: Float](x: U, y: U): U = math.min(x, y).asInstanceOf[U]
    }
    @SerialVersionUID(2142189527751553605L)
    implicit object IeeeOrdering extends IeeeOrdering
  }
  @migration(
    "  The default implicit ordering for floats now maintains consistency\n" +
    "  between its `compare` method and its `lt`, `min`, `equiv`, etc., methods,\n" +
    "  which means nonconforming to IEEE 754's behavior for -0.0F and NaN.\n" +
    "  The sort order of floats remains the same, however, with NaN at the end.\n" +
    "  Import Ordering.Float.IeeeOrdering to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Ordering$$Float$.html.", "2.13.0")
  @SerialVersionUID(-8500693657289762132L)
  implicit object DeprecatedFloatOrdering extends Float.TotalOrdering

  /** `Ordering`s for `Double`s.
   *
   *  The behavior of the comparison operations provided by the default (implicit)
   *  ordering on `Double` changed in 2.10.0 and 2.13.0.
   *  Prior to Scala 2.10.0, the `Ordering` instance used semantics
   *  consistent with `java.lang.Double.compare`.
   *
   *  Scala 2.10.0 changed the implementation of `lt`, `equiv`, `min`, etc., to be
   *  IEEE 754 compliant, while keeping the `compare` method NOT compliant,
   *  creating an internally inconsistent instance. IEEE 754 specifies that
   *  `0.0 == -0.0`. In addition, it requires all comparisons with `Double.NaN` return
   *  `false` thus `0.0 < Double.NaN`, `0.0 > Double.NaN`, and
   *  `Double.NaN == Double.NaN` all yield `false`, analogous `None` in `flatMap`.
   *
   *  Recognizing the limitation of the IEEE 754 semantics in terms of ordering,
   *  Scala 2.13.0 created two instances: `Ordering.Double.IeeeOrdering`, which retains
   *  the IEEE 754 semantics from Scala 2.12.x, and `Ordering.Double.TotalOrdering`,
   *  which brings back the `java.lang.Double.compare` semantics for all operations.
   *  The default extends `TotalOrdering`.
   *
  *  ```scala sc:compile
   *  List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).sorted      // List(-Infinity, 0.0, 1.0, NaN)
   *  List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).min         // -Infinity
   *  implicitly[Ordering[Double]].lt(0.0, 0.0 / 0.0)   // true
   *  {
   *    import Ordering.Double.IeeeOrdering
   *    List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).sorted    // List(-Infinity, 0.0, 1.0, NaN)
   *    List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).min       // NaN
   *    implicitly[Ordering[Double]].lt(0.0, 0.0 / 0.0) // false
   *  }
   *  ```
   *
   *  @define doubleOrdering Because the behavior of `Double`s specified by IEEE is
   *                        not consistent with a total ordering when dealing with
   *                        `NaN`, there are two orderings defined for `Double`:
   *                        `TotalOrdering`, which is consistent with a total
   *                        ordering, and `IeeeOrdering`, which is consistent
   *                        as much as possible with IEEE spec and floating point
   *                        operations defined in [[scala.math]].
   */
  object Double {
    /** An ordering for `Double`s which is a fully consistent total ordering,
     *  and treats `NaN` as larger than all other `Double` values; it behaves
     *  the same as [[java.lang.Double.compare]].
     *
     *  $doubleOrdering
     *
     *  This ordering may be preferable for sorting collections.
     *
     *  @see [[IeeeOrdering]]
     */
    trait TotalOrdering extends Ordering[Double] {
      /** Compares two `Double` values using the total ordering.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       */
      def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
    }
    @SerialVersionUID(-831119229746134011L)
    implicit object TotalOrdering extends TotalOrdering

    /** An ordering for `Double`s which is consistent with IEEE specifications
     *  whenever possible.
     *
     *   - `lt`, `lteq`, `equiv`, `gteq` and `gt` are consistent with primitive
     *  comparison operations for `Double`s, and return `false` when called with
     *  `NaN`.
     *   - `min` and `max` are consistent with `math.min` and `math.max`, and
     *  return `NaN` when called with `NaN` as either argument.
     *   - `compare` behaves the same as [[java.lang.Double.compare]].
     *
     *  $doubleOrdering
     *
     *  This ordering may be preferable for numeric contexts.
     *
     *  @see [[TotalOrdering]]
     */
    trait IeeeOrdering extends Ordering[Double] {
      /** Compares two `Double` values using the total ordering.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       */
      def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)

      /** Returns true if `x` is less than or equal to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` <= `y`, `false` otherwise
       */
      override def lteq(x: Double, y: Double): Boolean = x <= y
      /** Returns true if `x` is greater than or equal to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` >= `y`, `false` otherwise
       */
      override def gteq(x: Double, y: Double): Boolean = x >= y
      /** Returns true if `x` is less than `y` using IEEE 754 semantics.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` < `y`, `false` otherwise
       */
      override def lt(x: Double, y: Double): Boolean = x < y
      /** Returns true if `x` is greater than `y` using IEEE 754 semantics.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` > `y`, `false` otherwise
       */
      override def gt(x: Double, y: Double): Boolean = x > y
      /** Returns true if `x` is equivalent to `y` using IEEE 754 semantics.
       *
       *  @param x the first `Double` value to compare
       *  @param y the second `Double` value to compare
       *  @return `true` if `x` == `y`, `false` otherwise
       */
      override def equiv(x: Double, y: Double): Boolean = x == y
      /** Returns the maximum of `x` and `y` using IEEE 754 semantics.
       *
       *  @tparam U a subtype of `Double`, used to preserve the specific type in the return value
       *  @param x the first candidate value
       *  @param y the second candidate value
       *  @return the maximum of `x` and `y`
       */
      override def max[U <: Double](x: U, y: U): U = math.max(x, y).asInstanceOf[U]
      /** Returns the minimum of `x` and `y` using IEEE 754 semantics.
       *
       *  @tparam U a subtype of `Double`, used to preserve the specific type in the return value
       *  @param x the first candidate value
       *  @param y the second candidate value
       *  @return the minimum of `x` and `y`
       */
      override def min[U <: Double](x: U, y: U): U = math.min(x, y).asInstanceOf[U]
    }
    @SerialVersionUID(5722631152457877238L)
    implicit object IeeeOrdering extends IeeeOrdering
  }
  @migration(
    "  The default implicit ordering for doubles now maintains consistency\n" +
    "  between its `compare` method and its `lt`, `min`, `equiv`, etc., methods,\n" +
    "  which means nonconforming to IEEE 754's behavior for -0.0 and NaN.\n" +
    "  The sort order of doubles remains the same, however, with NaN at the end.\n" +
    "  Import Ordering.Double.IeeeOrdering to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Ordering$$Double$.html.", "2.13.0")
  @SerialVersionUID(-7340686892557971538L)
  implicit object DeprecatedDoubleOrdering extends Double.TotalOrdering

  /** An ordering for `BigInt` values.
   */
  trait BigIntOrdering extends Ordering[BigInt] {
    /** Compares two `BigInt` values.
     *
     *  @param x the first `BigInt` value to compare
     *  @param y the second `BigInt` value to compare
     */
    def compare(x: BigInt, y: BigInt) = x.compare(y)
  }
  @SerialVersionUID(-3075297647817530785L)
  implicit object BigInt extends BigIntOrdering

  /** An ordering for `BigDecimal` values.
   */
  trait BigDecimalOrdering extends Ordering[BigDecimal] {
    /** Compares two `BigDecimal` values.
     *
     *  @param x the first `BigDecimal` value to compare
     *  @param y the second `BigDecimal` value to compare
     */
    def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)
  }
  @SerialVersionUID(-833457937756812905L)
  implicit object BigDecimal extends BigDecimalOrdering

  /** An ordering for `String` values.
   */
  trait StringOrdering extends Ordering[String] {
    /** Compares two `String` values.
     *
     *  @param x the first `String` value to compare
     *  @param y the second `String` value to compare
     */
    def compare(x: String, y: String) = x.compareTo(y)
  }
  @SerialVersionUID(1302240016074071079L)
  implicit object String extends StringOrdering

  /** An ordering for `Symbol` values.
   */
  trait SymbolOrdering extends Ordering[Symbol] {
    /** Compares two `Symbol` values by their names.
     *
     *  @param x the first `Symbol` value to compare
     *  @param y the second `Symbol` value to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: Symbol, y: Symbol): Int = x.name.compareTo(y.name)
  }
  @SerialVersionUID(1996702162912307637L)
  implicit object Symbol extends SymbolOrdering

  /** An ordering for `Option` values.
   *
   *  @tparam T the type of the values contained in the `Option`
   */
  trait OptionOrdering[T] extends Ordering[Option[T]] {
    /** The ordering used to compare the values contained in the `Option`.
     */
    def optionOrdering: Ordering[T]
    /** Compares two `Option` values.
     *
     *  @param x the first `Option` value to compare
     *  @param y the second `Option` value to compare
     */
    def compare(x: Option[T], y: Option[T]) = (x, y) match {
      case (None, None)       => 0
      case (None, _)          => -1
      case (_, None)          => 1
      case (Some(x), Some(y)) => optionOrdering.compare(x, y)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: OptionOrdering[?]      => this.optionOrdering == that.optionOrdering
      case _                            => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = optionOrdering.hashCode() * optionSeed
  }
  /** Creates an ordering for `Option` values using the given element ordering.
   *
   *  @tparam T the type of the values contained in the `Option`
   *  @param ord the implicit `Ordering` used to compare the values contained in the `Option`
   *  @return an `Ordering[Option[T]]` that compares `Option` values using `ord`
   */
  implicit def Option[T](implicit ord: Ordering[T]): Ordering[Option[T]] = {
    @SerialVersionUID(6958068162830323876L)
    class O extends  OptionOrdering[T] { val optionOrdering = ord }
    new O()
  }

  /**
   *  @deprecated Iterables are not guaranteed to have a consistent order, so the `Ordering`
   *             returned by this method may not be stable or meaningful. If you are using a type
   *             with a consistent order (such as `Seq`), use its `Ordering` (found in the
   *             [[Implicits]] object) instead.
   */
  @deprecated("Iterables are not guaranteed to have a consistent order; if using a type with a " +
    "consistent order (e.g. Seq), use its Ordering (found in the Ordering.Implicits object)", since = "2.13.0")
  /** Creates an ordering for `Iterable` values using the given element ordering.
   *
   *  @tparam T the type of the elements contained in the `Iterable`
   *  @param ord the implicit `Ordering` used to compare elements of type `T`
   *  @return an `Ordering[Iterable[T]]` that compares `Iterable` values lexicographically using `ord`
   */
  implicit def Iterable[T](implicit ord: Ordering[T]): Ordering[Iterable[T]] =
    new IterableOrdering[Iterable, T](ord)

  /** Creates an ordering for tuples of two elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @return an `Ordering[(T1, T2)]` that compares tuples lexicographically using `ord1` and `ord2`
   */
  implicit def Tuple2[T1, T2](implicit ord1: Ordering[T1], ord2: Ordering[T2]): Ordering[(T1, T2)] =
    new Tuple2Ordering(ord1, ord2)

  @SerialVersionUID(4945084135299531202L)
  private final class Tuple2Ordering[T1, T2](private val ord1: Ordering[T1],
                                                   private val ord2: Ordering[T2]) extends Ordering[(T1, T2)] {
    /** Compares two tuples of two elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2), y: (T1, T2)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      ord2.compare(x._2, y._2)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple2Ordering[?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2).hashCode()
  }

  /** Creates an ordering for tuples of three elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @return an `Ordering[(T1, T2, T3)]` that compares tuples lexicographically using `ord1`, `ord2`, and `ord3`
   */
  implicit def Tuple3[T1, T2, T3](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3]) : Ordering[(T1, T2, T3)] =
    new Tuple3Ordering(ord1, ord2, ord3)

  @SerialVersionUID(-5367223704121832335L)
  private final class Tuple3Ordering[T1, T2, T3](private val ord1: Ordering[T1],
                                                       private val ord2: Ordering[T2],
                                                       private val ord3: Ordering[T3]) extends Ordering[(T1, T2, T3)] {
    /** Compares two tuples of three elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3), y: (T1, T2, T3)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      ord3.compare(x._3, y._3)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple3Ordering[?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3).hashCode()
  }

  /** Creates an ordering for tuples of four elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @return an `Ordering[(T1, T2, T3, T4)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, and `ord4`
   */
  implicit def Tuple4[T1, T2, T3, T4](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4]) : Ordering[(T1, T2, T3, T4)] =
    new Tuple4Ordering(ord1, ord2, ord3, ord4)

  @SerialVersionUID(-6055313861145218178L)
  private final class Tuple4Ordering[T1, T2, T3, T4](private val ord1: Ordering[T1],
                                                           private val ord2: Ordering[T2],
                                                           private val ord3: Ordering[T3],
                                                           private val ord4: Ordering[T4])
    extends Ordering[(T1, T2, T3, T4)] {
    /** Compares two tuples of four elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4), y: (T1, T2, T3, T4)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      ord4.compare(x._4, y._4)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple4Ordering[?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4).hashCode()
  }

  /** Creates an ordering for tuples of five elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @param ord5 the implicit `Ordering` used to compare the fifth element
   *  @return an `Ordering[(T1, T2, T3, T4, T5)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, `ord4`, and `ord5`
   */
  implicit def Tuple5[T1, T2, T3, T4, T5](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5]): Ordering[(T1, T2, T3, T4, T5)] =
    new Tuple5Ordering(ord1, ord2, ord3, ord4, ord5)

  @SerialVersionUID(-5517329921227646061L)
  private final class Tuple5Ordering[T1, T2, T3, T4, T5](private val ord1: Ordering[T1],
                                                               private val ord2: Ordering[T2],
                                                               private val ord3: Ordering[T3],
                                                               private val ord4: Ordering[T4],
                                                               private val ord5: Ordering[T5])
    extends Ordering[(T1, T2, T3, T4, T5)] {
    /** Compares two tuples of five elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4, T5), y: (T1, T2, T3, T4, T5)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      ord5.compare(x._5, y._5)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple5Ordering[?, ?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5).hashCode()
  }

  @SerialVersionUID(3045467524192969060L)
  /** Creates an ordering for tuples of six elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @param ord5 the implicit `Ordering` used to compare the fifth element
   *  @param ord6 the implicit `Ordering` used to compare the sixth element
   *  @return an `Ordering[(T1, T2, T3, T4, T5, T6)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, `ord4`, `ord5`, and `ord6`
   */
  implicit def Tuple6[T1, T2, T3, T4, T5, T6](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6]): Ordering[(T1, T2, T3, T4, T5, T6)] =
    new Tuple6Ordering(ord1, ord2, ord3, ord4, ord5, ord6)

  private final class Tuple6Ordering[T1, T2, T3, T4, T5, T6](private val ord1: Ordering[T1],
                                                                   private val ord2: Ordering[T2],
                                                                   private val ord3: Ordering[T3],
                                                                   private val ord4: Ordering[T4],
                                                                   private val ord5: Ordering[T5],
                                                                   private val ord6: Ordering[T6])
    extends Ordering[(T1, T2, T3, T4, T5, T6)] {
    /** Compares two tuples of six elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4, T5, T6), y: (T1, T2, T3, T4, T5, T6)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      ord6.compare(x._6, y._6)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple6Ordering[?, ?, ?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6).hashCode()
  }

  /** Creates an ordering for tuples of seven elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @tparam T7 the type of the seventh element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @param ord5 the implicit `Ordering` used to compare the fifth element
   *  @param ord6 the implicit `Ordering` used to compare the sixth element
   *  @param ord7 the implicit `Ordering` used to compare the seventh element
   *  @return an `Ordering[(T1, T2, T3, T4, T5, T6, T7)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, `ord4`, `ord5`, `ord6`, and `ord7`
   */
  implicit def Tuple7[T1, T2, T3, T4, T5, T6, T7](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7]): Ordering[(T1, T2, T3, T4, T5, T6, T7)] =
    new Tuple7Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7)

  @SerialVersionUID(1253188205893682451L)
  private final class Tuple7Ordering[T1, T2, T3, T4, T5, T6, T7](private val ord1: Ordering[T1],
                                                                       private val ord2: Ordering[T2],
                                                                       private val ord3: Ordering[T3],
                                                                       private val ord4: Ordering[T4],
                                                                       private val ord5: Ordering[T5],
                                                                       private val ord6: Ordering[T6],
                                                                       private val ord7: Ordering[T7])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7)] {
    /** Compares two tuples of seven elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4, T5, T6, T7), y: (T1, T2, T3, T4, T5, T6, T7)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      ord7.compare(x._7, y._7)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple7Ordering[?, ?, ?, ?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7).hashCode()
  }

  @SerialVersionUID(4003095353309354068L)
  /** Creates an ordering for tuples of eight elements using the given element orderings.
   *
   *  @tparam T1 the type of the first element in the tuple
   *  @tparam T2 the type of the second element in the tuple
   *  @tparam T3 the type of the third element in the tuple
   *  @tparam T4 the type of the fourth element in the tuple
   *  @tparam T5 the type of the fifth element in the tuple
   *  @tparam T6 the type of the sixth element in the tuple
   *  @tparam T7 the type of the seventh element in the tuple
   *  @tparam T8 the type of the eighth element in the tuple
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @param ord5 the implicit `Ordering` used to compare the fifth element
   *  @param ord6 the implicit `Ordering` used to compare the sixth element
   *  @param ord7 the implicit `Ordering` used to compare the seventh element
   *  @param ord8 the implicit `Ordering` used to compare the eighth element
   *  @return an `Ordering[(T1, T2, T3, T4, T5, T6, T7, T8)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, `ord4`, `ord5`, `ord6`, `ord7`, and `ord8`
   */
  implicit def Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7], ord8: Ordering[T8]): Ordering[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    new Tuple8Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8)

  private final class Tuple8Ordering[T1, T2, T3, T4, T5, T6, T7, T8](private val ord1: Ordering[T1],
                                                                           private val ord2: Ordering[T2],
                                                                           private val ord3: Ordering[T3],
                                                                           private val ord4: Ordering[T4],
                                                                           private val ord5: Ordering[T5],
                                                                           private val ord6: Ordering[T6],
                                                                           private val ord7: Ordering[T7],
                                                                           private val ord8: Ordering[T8])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    /** Compares two tuples of eight elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4, T5, T6, T7, T8), y: (T1, T2, T3, T4, T5, T6, T7, T8)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      val compare7 = ord7.compare(x._7, y._7)
      if (compare7 != 0) return compare7
      ord8.compare(x._8, y._8)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple8Ordering[?, ?, ?, ?, ?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7 &&
        this.ord8 == that.ord8
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8).hashCode()
  }

  @SerialVersionUID(8185342054829975001L)
  /** Creates an ordering for tuples of nine elements using the given element orderings.
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
   *  @param ord1 the implicit `Ordering` used to compare the first element
   *  @param ord2 the implicit `Ordering` used to compare the second element
   *  @param ord3 the implicit `Ordering` used to compare the third element
   *  @param ord4 the implicit `Ordering` used to compare the fourth element
   *  @param ord5 the implicit `Ordering` used to compare the fifth element
   *  @param ord6 the implicit `Ordering` used to compare the sixth element
   *  @param ord7 the implicit `Ordering` used to compare the seventh element
   *  @param ord8 the implicit `Ordering` used to compare the eighth element
   *  @param ord9 the implicit `Ordering` used to compare the ninth element
   *  @return an `Ordering[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]` that compares tuples lexicographically using `ord1`, `ord2`, `ord3`, `ord4`, `ord5`, `ord6`, `ord7`, `ord8`, and `ord9`
   */
  implicit def Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7], ord8 : Ordering[T8], ord9: Ordering[T9]): Ordering[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new Tuple9Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8, ord9)

  private final class Tuple9Ordering[T1, T2, T3, T4, T5, T6, T7, T8, T9](private val ord1: Ordering[T1],
                                                                               private val ord2: Ordering[T2],
                                                                               private val ord3: Ordering[T3],
                                                                               private val ord4: Ordering[T4],
                                                                               private val ord5: Ordering[T5],
                                                                               private val ord6: Ordering[T6],
                                                                               private val ord7: Ordering[T7],
                                                                               private val ord8: Ordering[T8],
                                                                               private val ord9: Ordering[T9])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    /** Compares two tuples of nine elements lexicographically.
     *
     *  @param x the first tuple to compare
     *  @param y the second tuple to compare
     *  @return a negative integer, zero, or a positive integer as `x` is less than, equal to, or greater than `y`
     */
    def compare(x: (T1, T2, T3, T4, T5, T6, T7, T8, T9), y: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      val compare7 = ord7.compare(x._7, y._7)
      if (compare7 != 0) return compare7
      val compare8 = ord8.compare(x._8, y._8)
      if (compare8 != 0) return compare8
      ord9.compare(x._9, y._9)
    }

    /** Returns whether the given object is equal to this ordering.
     *
     *  @param obj the object to compare with this ordering
     *  @return `true` if `obj` is equal to this ordering, `false` otherwise
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple9Ordering[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7 &&
        this.ord8 == that.ord8 &&
        this.ord9 == that.ord9
      case _ => false
    }
    /** Returns a hash code for this ordering.
     */
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8, ord9).hashCode()
  }
}
