/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.language.`2.13`

import scala.collection.{SpecificIterableFactory, StrictOptimizedIterableOps, View, immutable, mutable}
import java.lang.reflect.{Field => JField, Method => JMethod}

import scala.annotation.implicitNotFound
import scala.reflect.NameTransformer._
import scala.util.matching.Regex

/** Defines a finite set of values specific to the enumeration. Typically
 *  these values enumerate all possible forms something can take and provide
 *  a lightweight alternative to case classes.
 *
 *  Each call to a `Value` method adds a new unique value to the enumeration.
 *  To be accessible, these values are usually defined as `val` members of
 *  the enumeration.
 *
 *  All values in an enumeration share a common, unique type defined as the
 *  `Value` type member of the enumeration (`Value` selected on the stable
 *  identifier path of the enumeration instance).
 *
 *  Values SHOULD NOT be added to an enumeration after its construction;
 *  doing so makes the enumeration thread-unsafe. If values are added to an
 *  enumeration from multiple threads (in a non-synchronized fashion) after
 *  construction, the behavior of the enumeration is undefined.
 *
 *  @example ```
 *  // Define a new enumeration with a type alias and work with the full set of enumerated values
 *  object WeekDay extends Enumeration {
 *   type WeekDay = Value
 *   val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
 *  }
 *  import WeekDay._
 *
 *  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)
 *
 *  WeekDay.values filter isWorkingDay foreach println
 *  // output:
 *  // Mon
 *  // Tue
 *  // Wed
 *  // Thu
 *  // Fri
 *  ```
 *
 *  @example ```
 *  // Example of adding attributes to an enumeration by extending the Enumeration.Val class
 *  object Planet extends Enumeration {
 *   protected case class Val(mass: Double, radius: Double) extends super.Val {
 *     def surfaceGravity: Double = Planet.G * mass / (radius * radius)
 *     def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
 *   }
 *   import scala.language.implicitConversions
 *   implicit def valueToPlanetVal(x: Value): Val = x.asInstanceOf[Val]
 *
 *   val G: Double = 6.67300E-11
 *   val Mercury = Val(3.303e+23, 2.4397e6)
 *   val Venus   = Val(4.869e+24, 6.0518e6)
 *   val Earth   = Val(5.976e+24, 6.37814e6)
 *   val Mars    = Val(6.421e+23, 3.3972e6)
 *   val Jupiter = Val(1.9e+27, 7.1492e7)
 *   val Saturn  = Val(5.688e+26, 6.0268e7)
 *   val Uranus  = Val(8.686e+25, 2.5559e7)
 *   val Neptune = Val(1.024e+26, 2.4746e7)
 *  }
 *
 *  println(Planet.values.filter(_.radius > 7.0e6))
 *  // output:
 *  // Planet.ValueSet(Jupiter, Saturn, Uranus, Neptune)
 *  ```
 *
 *  @param initial The initial value from which to count the integers that
 *                 identifies values at run-time.
 *  @author  Matthias Zenger
 */
@SerialVersionUID(8476000850333817230L)
abstract class Enumeration (initial: Int) extends Serializable {
  thisenum =>

  /** Creates an enumeration whose value ids are counted from `0`. */
  def this() = this(0)

  /* Note that `readResolve` cannot be private, since otherwise
     the JVM does not invoke it when deserializing subclasses. */
  /** Serialization hook that on the JVM resolves a deserialized enumeration to
   *  its singleton module instance; not implemented on Scala.js.
   *
   *  @throws NotImplementedError always
   */
  protected def readResolve(): AnyRef = ???

  /** The name of this enumeration. */
  override def toString =
    (getClass.getName.stripSuffix("$").split('.')).last.split('$').last

  /** The mapping from the integer used to identify values to the actual
   *  values. 
   */
  private val vmap: mutable.Map[Int, Value] = new mutable.HashMap

  /** The cache listing all values of this enumeration. */
  @transient private var vset: ValueSet | Null = null
  @transient @volatile private var vsetDefined = false

  /** The mapping from the integer used to identify values to their
   *  names. 
   */
  private[this] val nmap: mutable.Map[Int, String] = new mutable.HashMap

  /** The values of this enumeration as a set. */
  def values: ValueSet = {
    if (!vsetDefined) {
      vset = (ValueSet.newBuilder ++= vmap.values).result()
      vsetDefined = true
    }
    vset.nn
  }

  /** The integer to use to identify the next created value. */
  protected var nextId: Int = initial

  /** The string to use to name the next created value. */
  protected var nextName: Iterator[String] = _

  private def nextNameOrNull: String | Null =
    if (nextName != null && nextName.hasNext) nextName.next() else null

  /** The highest integer amongst those used to identify values in this
   *  enumeration. 
   */
  private[this] var topId = initial

  /** The lowest integer amongst those used to identify values in this
   *  enumeration, but no higher than 0. 
   */
  private[this] var bottomId = if(initial < 0) initial else 0

  /** The one higher than the highest integer amongst those used to identify
   *  values in this enumeration. 
   */
  final def maxId = topId

  /** The value of this enumeration with given id `x`
   *
   *  @param x the integer id of the desired value
   */
  final def apply(x: Int): Value = vmap(x)

  /** Returns a `Value` from this `Enumeration` whose name matches
   *  the argument `s`.  The names are determined automatically via reflection.
   *
   *  @param  s an `Enumeration` name
   *  @return   the `Value` of this `Enumeration` if its name matches `s`
   *  @throws   NoSuchElementException if no `Value` with a matching
   *           name is in this `Enumeration`
   */
  final def withName(s: String): Value = {
    val (unnamed, named) = values partition {
      _.toString().startsWith("<Unknown name for enum field ")
    }

    named.find(_.toString == s) match {
      case Some(v) => v
      // If we have unnamed values, we issue a detailed error message
      case None if unnamed.nonEmpty =>
        throw new NoSuchElementException(
          s"""Couldn't find enum field with name $s.
             |However, there were the following unnamed fields:
             |${unnamed.mkString("  ","\n  ","")}""".stripMargin)
      // Normal case (no unnamed Values)
      case _ => None.get
    }
  }

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value = Value(nextId)

  /** Creates a fresh value, part of this enumeration, identified by the
   *  integer `i`.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  Fresh value identified by `i`.
   */
  protected final def Value(i: Int): Value = Value(i, nextNameOrNull)

  /** Creates a fresh value, part of this enumeration, called `name`.
   *
   *  @param name A human-readable name for that value.
   *  @return  Fresh value called `name`.
   */
  protected final def Value(name: String | Null): Value = Value(nextId, name)

  /** Creates a fresh value, part of this enumeration, called `name`
   *  and identified by the integer `i`.
   *
   *  @param i    An integer that identifies this value at run-time. It must be
   *             unique amongst all values of the enumeration.
   *  @param name A human-readable name for that value.
   *  @return     Fresh value with the provided identifier `i` and name `name`.
   */
  protected final def Value(i: Int, name: String | Null): Value = new Val(i, name)

  /** The type of the enumerated values. */
  @SerialVersionUID(7091335633555234129L)
  abstract class Value extends Ordered[Value] with Serializable {
    /** The id and bit location of this enumeration value. */
    def id: Int
    /** A marker so we can tell whose values belong to whom come reflective-naming time. */
    private[Enumeration] val outerEnum = thisenum

    /** Compares this value with another value of this enumeration by id.
     *
     *  @param that the value to compare with
     *  @return `-1`, `0`, or `1` as this value's id is less than, equal to, or
     *          greater than the id of `that`
     */
    override def compare(that: Value): Int =
      if (this.id < that.id) -1
      else if (this.id == that.id) 0
      else 1
    /** Returns `true` if `other` is a value of the same enumeration instance
     *  with the same id as this value.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any) = other match {
      case that: Enumeration#Value  => (outerEnum eq that.outerEnum) && (id == that.id)
      case _                        => false
    }
    /** Returns the hash code of this value, computed from its id. */
    override def hashCode: Int = id.##

    /** Creates a ValueSet which contains this value and another one.
     *
     *  @param v the other value to include in the set
     */
    def + (v: Value) = ValueSet(this, v)
  }

  /** A class implementing the [[scala.Enumeration.Value]] type. This class
   *  can be overridden to change the enumeration's naming and integer
   *  identification behaviour.
   */
  @SerialVersionUID(0 - 3501153230598116017L)
  protected class Val(i: Int, name: String | Null) extends Value with Serializable {
    /** Creates a fresh value identified by `i`, named by the next name in
     *  `nextName` if defined, or unnamed otherwise.
     *
     *  @param i the integer that identifies the value at run-time; must be
     *           unique amongst all values of the enumeration
     */
    def this(i: Int)       = this(i, nextNameOrNull)
    /** Creates a fresh value identified by `nextId`, called `name`.
     *
     *  @param name a human-readable name for the value, or `null` for an
     *              unnamed value
     */
    def this(name: String | Null) = this(nextId, name)
    /** Creates a fresh value identified by `nextId`, named by the next name in
     *  `nextName` if defined, or unnamed otherwise.
     */
    def this()             = this(nextId)

    assert(!vmap.isDefinedAt(i), "Duplicate id: " + i)
    vmap(i) = this
    vsetDefined = false
    nextId = i + 1
    if (nextId > topId) topId = nextId
    if (i < bottomId) bottomId = i
    /** The integer that identifies this value within its enumeration. */
    def id = i
    /** Returns the name of this value if one was provided or drawn from
     *  `nextName`, otherwise a placeholder of the form
     *  `<Unknown name for enum field #$i of class $cls>` (on Scala.js, names of
     *  unnamed values cannot be recovered by reflection).
     */
    override def toString() =
      if (name != null) name
      // Scala.js specific
      else s"<Unknown name for enum field #$i of class ${getClass}>"

    /** During deserialization, replaces this value with the equivalent value
     *  registered in the owning enumeration, as resolved by
     *  `Enumeration.readResolve` (which is unimplemented on Scala.js and throws
     *  `NotImplementedError` unless overridden).
     */
    protected def readResolve(): AnyRef = {
      val enumeration = thisenum.readResolve().asInstanceOf[Enumeration]
      if (enumeration.vmap == null) this
      else enumeration.vmap(i)
    }
  }

  /** An ordering by id for values of this set. */
  implicit object ValueOrdering extends Ordering[Value] {
    /** Compares two values of this enumeration by id.
     *
     *  @param x the first value to compare
     *  @param y the second value to compare
     *  @return `-1`, `0`, or `1` as the id of `x` is less than, equal to, or
     *          greater than the id of `y`
     */
    def compare(x: Value, y: Value): Int = x compare y
  }

  /** A class for sets of values.
   *  Iterating through this set will yield values in increasing order of their ids.
   *
   *  @param nnIds The set of ids of values (adjusted so that the lowest value does
   *    not fall below zero), organized as a `BitSet`.
   *  @define Coll `collection.immutable.SortedSet`
   */
  class ValueSet private[ValueSet] (private[this] var nnIds: immutable.BitSet)
    extends immutable.AbstractSet[Value]
      with immutable.SortedSet[Value]
      with immutable.SortedSetOps[Value, immutable.SortedSet, ValueSet]
      with StrictOptimizedIterableOps[Value, immutable.Set, ValueSet]
      with Serializable {

    /** Returns [[ValueOrdering]], the ordering of values by their ids. */
    implicit def ordering: Ordering[Value] = ValueOrdering
    /** Returns the values of this set whose ids lie within given bounds.
     *
     *  @param from the value whose id is the inclusive lower bound, or `None`
     *              for no lower bound
     *  @param until the value whose id is the exclusive upper bound, or `None`
     *               for no upper bound
     *  @return a `ValueSet` containing the values of this set within the bounds
     */
    def rangeImpl(from: Option[Value], until: Option[Value]): ValueSet =
      new ValueSet(nnIds.rangeImpl(from.map(_.id - bottomId), until.map(_.id - bottomId)))

    /** Returns the empty set of values of this enumeration. */
    override def empty = ValueSet.empty
    /** Returns the number of values in this set; always known, since the
     *  underlying bit set is finite.
     */
    override def knownSize: Int = nnIds.size
    /** Returns `true` if this set contains no values. */
    override def isEmpty: Boolean = nnIds.isEmpty
    /** Tests whether this set contains a given value.
     *
     *  @param v the value to test for membership
     */
    def contains(v: Value) = nnIds contains (v.id - bottomId)
    /** Returns a new `ValueSet` containing the values of this set and the given value.
     *
     *  @param value the value to add
     */
    def incl (value: Value) = new ValueSet(nnIds + (value.id - bottomId))
    /** Returns a new `ValueSet` containing the values of this set except the given value.
     *
     *  @param value the value to remove
     */
    def excl (value: Value) = new ValueSet(nnIds - (value.id - bottomId))
    /** Returns an iterator over the values in this set, in increasing order of their ids. */
    def iterator = nnIds.iterator map (id => thisenum.apply(bottomId + id))
    /** Returns an iterator over the values in this set whose ids are greater
     *  than or equal to that of `start`, in increasing order of their ids.
     *
     *  @param start the inclusive lower bound for the values to return
     *  @note NEEDS-HUMAN: unlike the JVM version, this implementation does not
     *        subtract `bottomId` from `start.id` before filtering the
     *        zero-adjusted ids in the underlying bit set, so the bound appears
     *        to be applied incorrectly when the enumeration has negative ids.
     */
    override def iteratorFrom(start: Value) = nnIds iteratorFrom start.id  map (id => thisenum.apply(bottomId + id))
    /** Returns the name used to prefix the string representation of this set:
     *  the enumeration's name followed by `.ValueSet`.
     */
    override def className = s"$thisenum.ValueSet"
    /** Creates a bit mask for the zero-adjusted ids in this set as a
     *  new array of longs 
     */
    def toBitMask: Array[Long] = nnIds.toBitMask

    /** Builds a `ValueSet` containing the values of the given collection.
     *
     *  @param coll the source of values for the new set
     */
    override protected def fromSpecific(coll: IterableOnce[Value]) = ValueSet.fromSpecific(coll)
    /** Returns a new builder that accumulates values into a `ValueSet`. */
    override protected def newSpecificBuilder = ValueSet.newBuilder

    /** Builds a new `ValueSet` by applying a function to each value of this set.
     *
     *  @param f the function to apply to each value
     *  @return a `ValueSet` of the results, ordered by their ids
     */
    def map(f: Value => Value): ValueSet = fromSpecific(new View.Map(this, f))
    /** Builds a new `ValueSet` by applying a function to each value of this set
     *  and collecting all values in the results.
     *
     *  @param f the function to apply to each value
     *  @return a `ValueSet` of all values produced by `f`, ordered by their ids
     */
    def flatMap(f: Value => IterableOnce[Value]): ValueSet = fromSpecific(new View.FlatMap(this, f))

    // necessary for disambiguation:
    /** Builds a new sorted set by applying a function to each value of this set.
     *
     *  @tparam B the element type of the returned set
     *  @param f the function to apply to each value
     *  @param ev the ordering for the elements of the returned set
     *  @return a sorted set of the results of applying `f` to each value
     */
    override def map[B](f: Value => B)(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].map[B](f)
    /** Builds a new sorted set by applying a function to each value of this set
     *  and collecting all elements in the results.
     *
     *  @tparam B the element type of the returned set
     *  @param f the function to apply to each value
     *  @param ev the ordering for the elements of the returned set
     *  @return a sorted set of all elements produced by `f`
     */
    override def flatMap[B](f: Value => IterableOnce[B])(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].flatMap[B](f)
    /** Builds a new sorted set of pairs formed from the values of this set and
     *  the corresponding elements of another collection, dropping whatever
     *  remains of the longer of the two.
     *
     *  @tparam B the type of the second element of each pair
     *  @param that the collection providing the second element of each pair
     *  @param ev the ordering for the pairs in the returned set
     *  @return a sorted set of corresponding pairs
     */
    override def zip[B](that: IterableOnce[B])(implicit @implicitNotFound(ValueSet.zipOrdMsg) ev: Ordering[(Value, B)]): immutable.SortedSet[(Value, B)] =
      super[SortedSet].zip[B](that)
    /** Builds a new sorted set by applying a partial function to each value of
     *  this set on which it is defined.
     *
     *  @tparam B the element type of the returned set
     *  @param pf the partial function to apply to each value
     *  @param ev the ordering for the elements of the returned set
     *  @return a sorted set of the results of applying `pf` to each value on
     *          which it is defined
     */
    override def collect[B](pf: PartialFunction[Value, B])(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].collect[B](pf)
  }

  /** A factory object for value sets. */
  @SerialVersionUID(3L)
  object ValueSet extends SpecificIterableFactory[Value, ValueSet] {
    private final val ordMsg = "No implicit Ordering[${B}] found to build a SortedSet[${B}]. You may want to upcast to a Set[Value] first by calling `unsorted`."
    private final val zipOrdMsg = "No implicit Ordering[${B}] found to build a SortedSet[(Value, ${B})]. You may want to upcast to a Set[Value] first by calling `unsorted`."

    /** The empty value set. */
    val empty = new ValueSet(immutable.BitSet.empty)
    /** A value set containing all the values for the zero-adjusted ids
     *  corresponding to the bits in an array. 
     *
     *  @param elems an array of `Long` values encoding the bit mask of value ids
     */
    def fromBitMask(elems: Array[Long]): ValueSet = new ValueSet(immutable.BitSet.fromBitMask(elems))
    /** A builder object for value sets. */
    def newBuilder: mutable.Builder[Value, ValueSet] = new mutable.Builder[Value, ValueSet] {
      private[this] val b = new mutable.BitSet
      def addOne (x: Value) = { b += (x.id - bottomId); this }
      def clear() = b.clear()
      def result() = new ValueSet(b.toImmutable)
    }
    /** Builds a `ValueSet` containing the values of the given collection.
     *
     *  @param it the source of values for the new set
     *  @return a `ValueSet` containing the values of `it`
     */
    def fromSpecific(it: IterableOnce[Value]): ValueSet =
      newBuilder.addAll(it).result()
  }
}
