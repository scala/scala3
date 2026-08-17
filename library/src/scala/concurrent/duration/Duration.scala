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

package scala.concurrent.duration

import scala.language.`2.13`
import java.lang.{ Double => JDouble }
import scala.collection.StringParsers

object Duration {

  /** Constructs a Duration from the given length and unit. Observe that nanosecond precision may be lost if
   *
   *  - the unit is NANOSECONDS
   *  - and the length has an absolute value greater than `2^53`
   *
   *  Infinite inputs (and NaN) are converted into [[Duration.Inf]], [[Duration.MinusInf]] and [[Duration.Undefined]], respectively.
   *
   *  @param length the duration length as a floating-point number
   *  @param unit the time unit in which `length` is measured
   *  @return a duration representing the given length in the given unit, possibly infinite or undefined
   *  @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */
  def apply(length: Double, unit: TimeUnit): Duration     = fromNanos(unit.toNanos(1) * length)

  /** Constructs a finite duration from the given length and time unit. The unit given is retained
   *  throughout calculations as long as possible, so that it can be retrieved later.
   *
   *  @param length the duration length as a whole number
   *  @param unit the time unit in which `length` is measured
   *  @return a finite duration of the given length in the given unit
   */
  def apply(length: Long, unit: TimeUnit): FiniteDuration = new FiniteDuration(length, unit)

  /** Constructs a finite duration from the given length and time unit, where the latter is
   *  looked up in a list of string representation. Valid choices are:
   *
   *  `d, day, h, hr, hour, m, min, minute, s, sec, second, ms, milli, millisecond, µs, micro, microsecond, ns, nano, nanosecond`
   *  and their pluralized forms (for every but the first mentioned form of each unit, i.e. no "ds", but "days").
   *
   *  @param length the duration length as a whole number
   *  @param unit the string representation of the time unit (e.g. `"ms"`, `"second"`, `"days"`)
   *  @return a finite duration of the given length with the resolved time unit
   */
  def apply(length: Long, unit: String): FiniteDuration   = new FiniteDuration(length,  Duration.timeUnit(unit))

  // Double stores 52 bits mantissa, but there is an implied '1' in front, making the limit 2^53
  // private final val maxPreciseDouble = 9007199254740992d // not used after https://github.com/scala/scala/pull/9233

  /** Parses String into Duration.  Format is `"<length><unit>"`, where
   *  whitespace is allowed before, between and after the parts. Infinities are
   *  designated by `"Inf"`, `"PlusInf"`, `"+Inf"`, `"Duration.Inf"` and `"-Inf"`, `"MinusInf"` or `"Duration.MinusInf"`.
   *  Undefined is designated by `"Duration.Undefined"`.
   *
   *  @param s the string to parse into a duration
   *  @return the duration represented by the string, possibly infinite or undefined
   *  @throws NumberFormatException if format is not parsable
   */
  def apply(s: String): Duration = {
    val s1: String = s filterNot (_.isWhitespace)
    s1 match {
      case "Inf" | "PlusInf" | "+Inf" | "Duration.Inf" => Inf
      case "MinusInf" | "-Inf" | "Duration.MinusInf"   => MinusInf
      case "Duration.Undefined"                        => Undefined
      case _                                           =>
        val unitName = s1.reverse.takeWhile(_.isLetter).reverse
        timeUnit get unitName match {
          case Some(unit) =>
            val valueStr = s1 dropRight unitName.length
            StringParsers.parseLong(valueStr).map(Duration(_, unit))
              .getOrElse(Duration(JDouble.parseDouble(valueStr), unit))
          case _          => throw new NumberFormatException("format error " + s)
        }
    }
  }

  // "ms milli millisecond" -> List("ms", "milli", "millis", "millisecond", "milliseconds")
  private def words(s: String) = s.trim.split("\\s+").toList
  private def expandLabels(labels: String): List[String] = {
    val hd :: rest = words(labels): @unchecked
    hd :: rest.flatMap(s => List(s, s + "s"))
  }
  private val timeUnitLabels = List(
    DAYS         -> "d day",
    HOURS        -> "h hr hour",
    MINUTES      -> "m min minute",
    SECONDS      -> "s sec second",
    MILLISECONDS -> "ms milli millisecond",
    MICROSECONDS -> "µs micro microsecond",
    NANOSECONDS  -> "ns nano nanosecond"
  )

  // TimeUnit => standard label
  /** A mapping from TimeUnit to its standard string representation.
   *
   *  @return a map from TimeUnit to its standard string representation
   */
  protected[duration] val timeUnitName: Map[TimeUnit, String] =
    timeUnitLabels.toMap.view.mapValues(s => words(s).last).toMap

  // Label => TimeUnit
  /** A mapping from string representations to TimeUnit values.
   *
   *  @return a map from string representations to TimeUnit values
   */
  protected[duration] val timeUnit: Map[String, TimeUnit] =
    timeUnitLabels.flatMap{ case (unit, names) => expandLabels(names) map (_ -> unit) }.toMap

  /** Extracts length and time unit out of a string, where the format must match the description for [[Duration$.apply(s:String)* apply(String)]].
   *  The extractor will not match for malformed strings or non-finite durations.
   *
   *  @param s the string to parse and extract a length and time unit from
   *  @return `Some` pair of length and time unit if the string parses to a finite duration, `None` otherwise
   */
  def unapply(s: String): Option[(Long, TimeUnit)] =
    ( try Some(apply(s)) catch { case _: RuntimeException => None } ) flatMap unapply

  /** Extracts length and time unit out of a duration, if it is finite.
   *
   *  @param d the duration to decompose into length and time unit
   *  @return `Some` pair of length and time unit if `d` is finite, `None` otherwise
   */
  def unapply(d: Duration): Option[(Long, TimeUnit)] =
    if (d.isFinite) Some((d.length, d.unit)) else None

  /** Constructs a possibly infinite or undefined Duration from the given number of nanoseconds.
   *
   *  - `Double.PositiveInfinity` is mapped to [[Duration.Inf]]
   *  - `Double.NegativeInfinity` is mapped to [[Duration.MinusInf]]
   *  - `Double.NaN` is mapped to [[Duration.Undefined]]
   *  - `-0d` is mapped to [[Duration.Zero]] (exactly like `0d`)
   *
   *  The semantics of the resulting Duration objects matches the semantics of their Double
   *  counterparts with respect to arithmetic operations.
   *
   *  @param nanos the number of nanoseconds as a floating-point value, may be infinite or NaN
   *  @return a duration corresponding to the given number of nanoseconds, possibly infinite or undefined
   *  @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */
  def fromNanos(nanos: Double): Duration = {
    if (nanos.isInfinite)
      if (nanos > 0) Inf else MinusInf
    else if (JDouble.isNaN(nanos))
      Undefined
    else if (nanos > Long.MaxValue || nanos < Long.MinValue)
      throw new IllegalArgumentException("trying to construct too large duration with " + nanos + "ns")
    else
      fromNanos(nanos.round)
  }

  private final val ns_per_µs  = 1000L
  private final val ns_per_ms  = ns_per_µs  * 1000
  private final val ns_per_s   = ns_per_ms  * 1000
  private final val ns_per_min = ns_per_s   * 60
  private final val ns_per_h   = ns_per_min * 60
  private final val ns_per_d   = ns_per_h   * 24

  /** Constructs a finite duration from the given number of nanoseconds. The
   *  result will have the coarsest possible time unit which can exactly express
   *  this duration.
   *
   *  @param nanos the number of nanoseconds
   *  @return a finite duration equal to `nanos` nanoseconds, expressed using the coarsest time unit that represents it exactly
   *  @throws IllegalArgumentException for `Long.MinValue` since that would lead to inconsistent behavior afterwards (cannot be negated)
   */
  def fromNanos(nanos: Long): FiniteDuration = {
         if (nanos % ns_per_d   == 0) Duration(nanos / ns_per_d  , DAYS)
    else if (nanos % ns_per_h   == 0) Duration(nanos / ns_per_h  , HOURS)
    else if (nanos % ns_per_min == 0) Duration(nanos / ns_per_min, MINUTES)
    else if (nanos % ns_per_s   == 0) Duration(nanos / ns_per_s  , SECONDS)
    else if (nanos % ns_per_ms  == 0) Duration(nanos / ns_per_ms , MILLISECONDS)
    else if (nanos % ns_per_µs  == 0) Duration(nanos / ns_per_µs , MICROSECONDS)
    else Duration(nanos, NANOSECONDS)
  }

  /** Preconstructed value of `0.days`. */
  // unit as coarse as possible to keep (_ + Zero) sane unit-wise
  val Zero: FiniteDuration = new FiniteDuration(0, DAYS)

  /** The Undefined value corresponds closely to Double.NaN:
   *
   *  - it is the result of otherwise invalid operations
   *  - it does not equal itself (according to `equals()`)
   *  - it compares greater than any other Duration apart from itself (for which `compare` returns 0)
   *
   *  The particular comparison semantics mirror those of Double.NaN.
   *
   *  ***Use [[eq]] when checking an input of a method against this value.***
   */
  val Undefined: Infinite = new Infinite {
    override def toString() = "Duration.Undefined"
    override def equals(other: Any): Boolean  = false
    override def +(other: Duration): Duration = this
    override def -(other: Duration): Duration = this
    override def *(factor: Double): Duration  = this
    override def /(factor: Double): Duration  = this
    override def /(other: Duration): Double   = Double.NaN
    def compare(other: Duration): Int         = if (other eq this) 0 else 1
    def unary_- : Duration = this
    def toUnit(unit: TimeUnit): Double = Double.NaN
    private def readResolve(): AnyRef = Undefined      // Instructs deserialization to use this same instance
  }

  /** Represents an infinite duration, either positive or negative.
   *
   *  Infinite durations behave similarly to Double.PositiveInfinity and Double.NegativeInfinity
   *  in arithmetic operations.
   */
  sealed abstract class Infinite extends Duration {
    /** Returns the sum of this infinite duration and another duration.
     *
     *  The result follows Double semantics for infinite values:
     *  - Adding two different infinite durations (Inf + MinusInf) results in Undefined
     *  - Adding an infinite duration with Undefined results in Undefined
     *  - Adding an infinite duration with a finite duration results in the same infinite duration
     *
     *  @param other the duration to add to this one
     *  @return the sum of the two durations according to Double semantics
     */
    def +(other: Duration): Duration = other match {
      case x if x eq Undefined      => Undefined
      case x: Infinite if x ne this => Undefined
      case _                        => this
    }
    /** Returns the difference between this infinite duration and another duration.
     *
     *  The result follows Double semantics for infinite values:
     *  - Subtracting an infinite duration from itself results in Undefined
     *  - Subtracting an infinite duration from Undefined results in Undefined
     *  - Subtracting a finite duration from an infinite duration results in the same infinite duration
     *
     *  @param other the duration to subtract from this one
     *  @return the difference of the two durations according to Double semantics
     */
    def -(other: Duration): Duration = other match {
      case x if x eq Undefined      => Undefined
      case x: Infinite if x eq this => Undefined
      case _                        => this
    }

    /** Returns this infinite duration multiplied by a scalar factor.
     *
     *  The result follows Double semantics for infinite values:
     *  - Multiplying by zero results in Undefined
     *  - Multiplying by NaN results in Undefined
     *  - Multiplying by a negative factor negates the infinite duration
     *  - Multiplying by a positive factor returns the same infinite duration
     *
     *  @param factor the scalar to multiply by
     *  @return the scaled infinite duration according to Double semantics
     */
    def *(factor: Double): Duration =
      if (factor == 0d || JDouble.isNaN(factor)) Undefined
      else if (factor < 0d) -this
      else this
    /** Returns this infinite duration divided by a scalar divisor.
     *
     *  The result follows Double semantics for infinite values:
     *  - Dividing by NaN or infinity results in Undefined
     *  - Dividing by a negative divisor negates the infinite duration
     *  - Dividing by a positive divisor returns the same infinite duration
     *
     *  @param divisor the scalar to divide by
     *  @return the divided infinite duration according to Double semantics
     */
    def /(divisor: Double): Duration =
      if (JDouble.isNaN(divisor) || divisor.isInfinite) Undefined
      else if ((divisor compare 0d) < 0) -this
      else this
    /** Returns the quotient of this infinite duration divided by another duration.
     *
     *  The result follows Double semantics for infinite values:
     *  - Dividing by another infinite duration results in NaN
     *  - Dividing by a finite duration results in positive or negative infinity
     *
     *  @param divisor the duration to divide by
     *  @return the quotient as a Double according to Double semantics
     */
    def /(divisor: Duration): Double = divisor match {
      case _: Infinite => Double.NaN
      case x           => Double.PositiveInfinity * (if ((this > Zero) ^ (divisor >= Zero)) -1 else 1)
    }

    /** Returns false since this is an infinite duration.
     *
     *  @return false
     */
    final def isFinite = false

    private def fail(what: String) = throw new IllegalArgumentException(s"$what not allowed on infinite Durations")
    /** Throws an IllegalArgumentException since infinite durations do not have a length.
     *
     *  @throws IllegalArgumentException always
     */
    final def length: Long    = fail("length")
    /** Throws an IllegalArgumentException since infinite durations do not have a time unit.
     *
     *  @throws IllegalArgumentException always
     */
    final def unit: TimeUnit  = fail("unit")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to nanoseconds.
     *
     *  @throws IllegalArgumentException always
     */
    final def toNanos: Long   = fail("toNanos")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to microseconds.
     *
     *  @throws IllegalArgumentException always
     */
    final def toMicros: Long  = fail("toMicros")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to milliseconds.
     *
     *  @throws IllegalArgumentException always
     */
    final def toMillis: Long  = fail("toMillis")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to seconds.
     *
     *  @throws IllegalArgumentException always
     */
    final def toSeconds: Long = fail("toSeconds")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to minutes.
     *
     *  @throws IllegalArgumentException always
     */
    final def toMinutes: Long = fail("toMinutes")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to hours.
     *
     *  @throws IllegalArgumentException always
     */
    final def toHours: Long   = fail("toHours")
    /** Throws an IllegalArgumentException since infinite durations cannot be converted to days.
     *
     *  @throws IllegalArgumentException always
     */
    final def toDays: Long    = fail("toDays")

    /** Returns this infinite duration unchanged since it has no coarser representation.
     *
     *  @return this infinite duration
     */
    final def toCoarsest: Duration = this
  }

  /** Infinite duration: greater than any other (apart from Undefined) and not equal to any other
   *  but itself. This value closely corresponds to Double.PositiveInfinity,
   *  matching its semantics in arithmetic operations.
   */
  val Inf: Infinite = new Infinite  {
    override def toString(): String      = "Duration.Inf"
    def compare(other: Duration): Int  = other match {
      case x if x eq Undefined => -1 // Undefined != Undefined
      case x if x eq this      => 0  // `case Inf` will include null checks in the byte code
      case _                   => 1
    }
    def unary_- : Duration             = MinusInf
    def toUnit(unit: TimeUnit): Double = Double.PositiveInfinity
    private def readResolve(): AnyRef  = Inf            // Instructs deserialization to use this same instance
  }

  /** Infinite duration: less than any other and not equal to any other
   *  but itself. This value closely corresponds to Double.NegativeInfinity,
   *  matching its semantics in arithmetic operations.
   */
  val MinusInf: Infinite = new Infinite {
    override def toString(): String      = "Duration.MinusInf"
    def compare(other: Duration): Int  = if (other eq this) 0 else -1
    def unary_- : Duration = Inf
    def toUnit(unit: TimeUnit): Double = Double.NegativeInfinity
    private def readResolve(): AnyRef  = MinusInf    // Instructs deserialization to use this same instance
  }

  // Java Factories

  /** Constructs a finite duration from the given length and time unit. The unit given is retained
   *  throughout calculations as long as possible, so that it can be retrieved later.
   *
   *  @param length the duration length as a whole number
   *  @param unit the time unit in which `length` is measured
   *  @return a finite duration of the given length in the given unit
   */
  def create(length: Long, unit: TimeUnit): FiniteDuration = apply(length, unit)
  /** Constructs a Duration from the given length and unit. Observe that nanosecond precision may be lost if
   *
   *  - the unit is NANOSECONDS
   *  - and the length has an absolute value greater than `2^53`
   *
   *  Infinite inputs (and NaN) are converted into [[Duration.Inf]], [[Duration.MinusInf]] and [[Duration.Undefined]], respectively.
   *
   *  @param length the duration length as a floating-point number
   *  @param unit the time unit in which `length` is measured
   *  @return a duration representing the given length in the given unit, possibly infinite or undefined
   *  @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */
  def create(length: Double, unit: TimeUnit): Duration     = apply(length, unit)
  /** Constructs a finite duration from the given length and time unit, where the latter is
   *  looked up in a list of string representation. Valid choices are:
   *
   *  `d, day, h, hour, min, minute, s, sec, second, ms, milli, millisecond, µs, micro, microsecond, ns, nano, nanosecond`
   *  and their pluralized forms (for every but the first mentioned form of each unit, i.e. no "ds", but "days").
   *
   *  @param length the duration length as a whole number
   *  @param unit the string representation of the time unit (e.g. `"ms"`, `"second"`, `"days"`)
   *  @return a finite duration of the given length with the resolved time unit
   */
  def create(length: Long, unit: String): FiniteDuration   = apply(length, unit)
  /** Parses String into Duration.  Format is `"<length><unit>"`, where
   *  whitespace is allowed before, between and after the parts. Infinities are
   *  designated by `"Inf"`, `"PlusInf"`, `"+Inf"` and `"-Inf"` or `"MinusInf"`.
   *
   *  @param s the string to parse into a duration
   *  @return the duration represented by the string, possibly infinite or undefined
   *  @throws NumberFormatException if format is not parsable
   */
  def create(s: String): Duration                          = apply(s)

  /** The natural ordering of durations matches the natural ordering for Double, including non-finite values. */
  implicit object DurationIsOrdered extends Ordering[Duration] {
    /** Compares two durations according to their natural ordering.
     *
     *  The ordering matches Double semantics, where Undefined is considered greater than all other durations.
     *
     *  @param a the first duration to compare
     *  @param b the second duration to compare
     *  @return a negative integer if a < b, zero if a == b, or a positive integer if a > b
     */
    def compare(a: Duration, b: Duration): Int = a compare b
  }
}

/** <h2>Utility for working with java.util.concurrent.TimeUnit durations.</h2>
 *
 *  ***This class is not meant as a general purpose representation of time, it is
 *  optimized for the needs of `scala.concurrent`.***
 *
 *  <h2>Basic Usage</h2>
 *
 *  <p/>
 *  Examples:
 *  ```scala sc:compile
 *  import scala.concurrent.duration.*
 *
 *  val duration = Duration(100, MILLISECONDS)
 *  val sameDuration = Duration(100, "millis")
 *
 *  duration.toNanos
 *  duration < 1.second
 *  duration <= Duration.Inf
 *  ```
 *
 *  ***Invoking inexpressible conversions (like calling `toSeconds` on an infinite duration) will throw an IllegalArgumentException.***
 *
 *  <p/>
 *  Implicits are also provided for Int, Long and Double. Example usage:
 *  ```scala sc:compile
 *  import scala.concurrent.duration.*
 *
 *  val duration = 100.millis
 *  ```
 *
 *  ***The DSL provided by the implicit conversions always allows construction of finite durations, even for infinite Double inputs; use Duration.Inf instead.***
 *
 *  Extractors, parsing and arithmetic are also included:
 *  ```scala sc:compile
 *  import scala.concurrent.duration.*
 *  import scala.language.postfixOps
 *  val d = Duration("1.2 µs")
 *  val Duration(length, unit) = 5 millis
 *  val d2 = d * 2.5
 *  val d3 = d2 + 1.millisecond
 *  ```
 *
 *  <h2>Handling of Time Units</h2>
 *
 *  Calculations performed on finite durations always retain the more precise unit of either operand, no matter
 *  whether a coarser unit would be able to exactly express the same duration. This means that Duration can be
 *  used as a lossless container for a (length, unit) pair if it is constructed using the corresponding methods
 *  and no arithmetic is performed on it; adding/subtracting durations should in that case be done with care.
 *
 *  <h2>Correspondence to Double Semantics</h2>
 *
 *  The semantics of arithmetic operations on Duration are two-fold:
 *
 *  - exact addition/subtraction with nanosecond resolution for finite durations, independent of the summands' magnitude
 *  - isomorphic to `java.lang.Double` when it comes to infinite or undefined values
 *
 *  The conversion between Duration and Double is done using [[Duration.toUnit]] (with unit NANOSECONDS)
 *  and [[Duration$.fromNanos(nanos:Double)* Duration.fromNanos(Double)]]
 *
 *  <h2>Ordering</h2>
 *
 *  The default ordering is consistent with the ordering of Double numbers, which means that Undefined is
 *  considered greater than all other durations, including [[Duration.Inf]].
 *
 *  @define exc @throws IllegalArgumentException when invoked on a non-finite duration
 *
 *  @define ovf @throws IllegalArgumentException in case of a finite overflow: the range of a finite duration is `+-(2^63-1)`ns, and no conversion to infinite durations takes place.
 */
sealed abstract class Duration extends Serializable with Ordered[Duration] {
  /** Obtains the length of this Duration measured in the unit obtained by the `unit` method.
   *
   *  $exc
   *
   *  @return the numeric length of this duration, measured in its associated `unit`
   */
  def length: Long
  /** Obtains the time unit in which the length of this duration is measured.
   *
   *  $exc
   *
   *  @return the time unit used to measure this duration
   */
  def unit: TimeUnit
  /** Returns the length of this duration measured in whole nanoseconds, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in nanoseconds
   */
  def toNanos: Long
  /** Returns the length of this duration measured in whole microseconds, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in microseconds
   */
  def toMicros: Long
  /** Returns the length of this duration measured in whole milliseconds, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in milliseconds
   */
  def toMillis: Long
  /** Returns the length of this duration measured in whole seconds, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in seconds
   */
  def toSeconds: Long
  /** Returns the length of this duration measured in whole minutes, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in minutes
   */
  def toMinutes: Long
  /** Returns the length of this duration measured in whole hours, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in hours
   */
  def toHours: Long
  /** Returns the length of this duration measured in whole days, rounding towards zero.
   *
   *  $exc
   *
   *  @return the length of this duration in days
   */
  def toDays: Long
  /** Returns the number of nanoseconds as floating point number, scaled down to the given unit.
   *  The result may not precisely represent this duration due to the Double datatype's inherent
   *  limitations (mantissa size effectively 53 bits). Non-finite durations are represented as
   *  - [[Duration.Undefined]] is mapped to Double.NaN
   *  - [[Duration.Inf]] is mapped to Double.PositiveInfinity
   *  - [[Duration.MinusInf]] is mapped to Double.NegativeInfinity
   *
   *  @param unit the time unit to convert to
   *  @return the length of this duration expressed in the given `unit` as a `Double`
   */
  def toUnit(unit: TimeUnit): Double

  /** Returns the sum of that duration and this. When involving non-finite summands the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param other the duration to add to this one
   *  @return the sum of the two durations
   */
  def +(other: Duration): Duration
  /** Returns the difference of that duration and this. When involving non-finite summands the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param other the duration to subtract from this one
   *  @return the difference of the two durations
   */
  def -(other: Duration): Duration
  /** Returns this duration multiplied by the scalar factor. When involving non-finite factors the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param factor the scalar to multiply by
   *  @return this duration scaled by the given factor
   */
  def *(factor: Double): Duration
  /** Returns this duration divided by the scalar factor. When involving non-finite factors the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param divisor the scalar to divide by
   *  @return this duration divided by the given divisor
   */
  def /(divisor: Double): Duration
  /** Returns the quotient of this and that duration as floating-point number. The semantics are
   *  determined by Double as if calculating the quotient of the nanosecond lengths of both factors.
   *
   *  @param divisor the duration to divide by
   */
  def /(divisor: Duration): Double
  /** Negate this duration. The only two values which are mapped to themselves are [[Duration.Zero]] and [[Duration.Undefined]]. */
  def unary_- : Duration
  /** This method returns whether this duration is finite, which is not the same as
   *  `!isInfinite` for Double because this method also returns `false` for [[Duration.Undefined]].
   */
  def isFinite: Boolean
  /** Returns the smaller of this and that duration as determined by the natural ordering.
   *
   *  @param other the duration to compare with
   */
  def min(other: Duration): Duration = if (this < other) this else other
  /** Returns the larger of this and that duration as determined by the natural ordering.
   *
   *  @param other the duration to compare with
   */
  def max(other: Duration): Duration = if (this > other) this else other

  // Java API

  /** Returns this duration divided by the scalar factor. When involving non-finite factors the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param divisor the scalar to divide by
   *  @return this duration divided by the given divisor
   */
  def div(divisor: Double): Duration = this / divisor
  /** Returns the quotient of this and that duration as floating-point number. The semantics are
   *  determined by Double as if calculating the quotient of the nanosecond lengths of both factors.
   *
   *  @param other the duration to divide by
   *  @return the quotient of this and `other` as a floating-point number
   */
  def div(other: Duration): Double   = this / other
  /** Returns true if this duration is greater than another duration.
   *
   *  @param other the duration to compare with
   *  @return true if this duration is greater than the other, false otherwise
   */
  def gt(other: Duration): Boolean   = this > other
  /** Returns true if this duration is greater than or equal to another duration.
   *
   *  @param other the duration to compare with
   *  @return true if this duration is greater than or equal to the other, false otherwise
   */
  def gteq(other: Duration): Boolean = this >= other
  /** Returns true if this duration is less than another duration.
   *
   *  @param other the duration to compare with
   *  @return true if this duration is less than the other, false otherwise
   */
  def lt(other: Duration): Boolean   = this < other
  /** Returns true if this duration is less than or equal to another duration.
   *
   *  @param other the duration to compare with
   *  @return true if this duration is less than or equal to the other, false otherwise
   */
  def lteq(other: Duration): Boolean = this <= other
  /** Returns the difference of that duration and this. When involving non-finite summands the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param other the duration to subtract from this one
   *  @return the difference of the two durations
   */
  def minus(other: Duration): Duration = this - other
  /** Returns this duration multiplied by the scalar factor. When involving non-finite factors the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param factor the scalar to multiply by
   *  @return this duration scaled by the given factor
   */
  def mul(factor: Double): Duration    = this * factor
  /** Negate this duration. The only two values which are mapped to themselves are [[Duration.Zero]] and [[Duration.Undefined]]. */
  def neg(): Duration                  = -this
  /** Returns the sum of that duration and this. When involving non-finite summands the semantics match those
   *  of Double.
   *
   *  $ovf
   *
   *  @param other the duration to add to this one
   *  @return the sum of the two durations
   */
  def plus(other: Duration): Duration  = this + other
  /** Returns duration which is equal to this duration but with a coarsest Unit, or self in case it is already the coarsest Unit
   *  <p/>
   *  Examples:
   *  ```scala sc:compile
   *  import scala.concurrent.duration.*
   *  Duration(60, MINUTES).toCoarsest // Duration(1, HOURS)
   *  Duration(1000, MILLISECONDS).toCoarsest // Duration(1, SECONDS)
   *  Duration(48, HOURS).toCoarsest // Duration(2, DAYS)
   *  Duration(5, SECONDS).toCoarsest // Duration(5, SECONDS)
   *  ```
   */
  def toCoarsest: Duration
}

object FiniteDuration {

  implicit object FiniteDurationIsOrdered extends Ordering[FiniteDuration] {
    /** Compares two finite durations according to their natural ordering.
     *
     *  @param a the first finite duration to compare
     *  @param b the second finite duration to compare
     *  @return a negative integer if a < b, zero if a == b, or a positive integer if a > b
     */
    def compare(a: FiniteDuration, b: FiniteDuration): Int = a compare b
  }

  /** Creates a finite duration with the given length and time unit.
   *
   *  @param length the duration length as a whole number
   *  @param unit the time unit in which `length` is measured
   *  @return a finite duration of the given length in the given unit
   */
  def apply(length: Long, unit: TimeUnit): FiniteDuration  = new FiniteDuration(length, unit)
  /** Creates a finite duration with the given length and time unit string.
   *
   *  @param length the duration length as a whole number
   *  @param unit the string representation of the time unit (e.g. "ms", "second", "days")
   *  @return a finite duration of the given length with the resolved time unit
   */
  def apply(length: Long, unit: String): FiniteDuration    = new FiniteDuration(length, Duration.timeUnit(unit))

  // limit on abs. value of durations in their units
  private final val max_ns = Long.MaxValue
  private final val max_µs = max_ns  / 1000
  private final val max_ms = max_µs  / 1000
  private final val max_s  = max_ms  / 1000
  private final val max_min= max_s   / 60
  private final val max_h  = max_min / 60
  private final val max_d  = max_h   / 24
}

/** This class represents a finite duration. Its addition and subtraction operators are overloaded to retain
 *  this guarantee statically. The range of this class is limited to `+-(2^63-1)`ns, which is roughly 292  years.
 *
 *  @param length the number of time units in this duration
 *  @param unit the time unit in which `length` is measured
 */
final class FiniteDuration(val length: Long, val unit: TimeUnit) extends Duration {
  import FiniteDuration._
  import Duration._

  private def bounded(max: Long) = -max <= length && length <= max

  require(unit match {
      /*
       * enforce the 2^63-1 ns limit, must be pos/neg symmetrical because of unary_-
       */
      case NANOSECONDS  => bounded(max_ns)
      case MICROSECONDS => bounded(max_µs)
      case MILLISECONDS => bounded(max_ms)
      case SECONDS      => bounded(max_s)
      case MINUTES      => bounded(max_min)
      case HOURS        => bounded(max_h)
      case DAYS         => bounded(max_d)
    }, "Duration is limited to +-(2^63-1)ns (ca. 292 years)")

  /** Returns the length of this duration measured in whole nanoseconds.
   *
   *  @return the length of this duration in nanoseconds
   */
  def toNanos: Long               = unit.toNanos(length)
  /** Returns the length of this duration measured in whole microseconds.
   *
   *  @return the length of this duration in microseconds
   */
  def toMicros: Long              = unit.toMicros(length)
  /** Returns the length of this duration measured in whole milliseconds.
   *
   *  @return the length of this duration in milliseconds
   */
  def toMillis: Long              = unit.toMillis(length)
  /** Returns the length of this duration measured in whole seconds.
   *
   *  @return the length of this duration in seconds
   */
  def toSeconds: Long             = unit.toSeconds(length)
  /** Returns the length of this duration measured in whole minutes.
   *
   *  @return the length of this duration in minutes
   */
  def toMinutes: Long             = unit.toMinutes(length)
  /** Returns the length of this duration measured in whole hours.
   *
   *  @return the length of this duration in hours
   */
  def toHours: Long               = unit.toHours(length)
  /** Returns the length of this duration measured in whole days.
   *
   *  @return the length of this duration in days
   */
  def toDays: Long                = unit.toDays(length)
  /** Returns the length of this duration expressed in the given time unit as a Double.
   *
   *  @param u the time unit to convert to
   *  @return the length of this duration expressed in the given unit as a Double
   */
  def toUnit(u: TimeUnit): Double = toNanos.toDouble / NANOSECONDS.convert(1, u)

  /** Constructs a [[Deadline]] from this duration by adding it to the current instant `Deadline.now`. */
  def fromNow: Deadline = Deadline.now + this

  private def unitString  = timeUnitName(unit) + ( if (length == 1) "" else "s" )
  /** Returns a string representation of this duration in the format "length unit".
   *
   *  @return a string representation of this duration
   */
  override def toString(): String     = "" + length + " " + unitString

  /** Compares this finite duration with another duration.
   *
   *  @param other the duration to compare with
   *  @return a negative integer if this < other, zero if this == other, or a positive integer if this > other
   */
  def compare(other: Duration): Int = other match {
    case x: FiniteDuration => toNanos compare x.toNanos
    case _                 => -(other compare this)
  }

  // see https://www.securecoding.cert.org/confluence/display/java/NUM00-J.+Detect+or+prevent+integer+overflow
  private def safeAdd(a: Long, b: Long): Long = {
    if ((b > 0) && (a > Long.MaxValue - b) ||
        (b < 0) && (a < Long.MinValue - b)) throw new IllegalArgumentException("integer overflow")
    a + b
  }
  private def add(otherLength: Long, otherUnit: TimeUnit): FiniteDuration = {
    val commonUnit = if (otherUnit.convert(1, unit) == 0) unit else otherUnit
    val totalLength = safeAdd(commonUnit.convert(length, unit), commonUnit.convert(otherLength, otherUnit))
    new FiniteDuration(totalLength, commonUnit)
  }

  /** Returns the sum of this finite duration and another duration.
   *
   *  @param other the duration to add to this one
   *  @return the sum of the two durations
   */
  def +(other: Duration): Duration = other match {
    case x: FiniteDuration => add(x.length, x.unit)
    case _                 => other
  }
  /** Returns the difference between this finite duration and another duration.
   *
   *  @param other the duration to subtract from this one
   *  @return the difference of the two durations
   */
  def -(other: Duration): Duration = other match {
    case x: FiniteDuration => add(-x.length, x.unit)
    case _                 => -other
  }

  /** Returns this finite duration multiplied by a scalar factor.
   *
   *  @param factor the scalar to multiply by
   *  @return the scaled duration
   */
  def *(factor: Double): Duration  =
    if (!factor.isInfinite) fromNanos(toNanos * factor)
    else if (JDouble.isNaN(factor)) Undefined
    else if ((factor > 0) ^ (this < Zero)) Inf
    else MinusInf

  /** Returns this finite duration divided by a scalar divisor.
   *
   *  @param divisor the scalar to divide by
   *  @return the divided duration
   */
  def /(divisor: Double): Duration =
    if (!divisor.isInfinite) fromNanos(toNanos / divisor)
    else if (JDouble.isNaN(divisor)) Undefined
    else Zero

  // if this is made a constant, then scalac will elide the conditional and always return +0.0, scala/bug#6331
  private def minusZero = -0d
  /** Returns the quotient of this finite duration divided by another duration.
   *
   *  @param divisor the duration to divide by
   *  @return the quotient as a Double
   */
  def /(divisor: Duration): Double =
    if (divisor.isFinite) toNanos.toDouble / divisor.toNanos
    else if (divisor eq Undefined) Double.NaN
    else if ((length < 0) ^ (divisor > Zero)) 0d
    else minusZero

  // overloaded methods taking FiniteDurations, so that you can calculate while statically staying finite
  /** Returns the sum of this finite duration and another finite duration.
   *
   *  @param other the finite duration to add to this one
   *  @return the sum of the two finite durations
   */
  def +(other: FiniteDuration): FiniteDuration     = add(other.length, other.unit)
  /** Returns the difference between this finite duration and another finite duration.
   *
   *  @param other the finite duration to subtract from this one
   *  @return the difference of the two finite durations
   */
  def -(other: FiniteDuration): FiniteDuration     = add(-other.length, other.unit)
  /** Returns the sum of this finite duration and another finite duration.
   *
   *  @param other the finite duration to add to this one
   *  @return the sum of the two finite durations
   */
  def plus(other: FiniteDuration): FiniteDuration  = this + other
  /** Returns the difference between this finite duration and another finite duration.
   *
   *  @param other the finite duration to subtract from this one
   *  @return the difference of the two finite durations
   */
  def minus(other: FiniteDuration): FiniteDuration = this - other
  /** Returns the smaller of this finite duration and another finite duration.
   *
   *  @param other the finite duration to compare with
   *  @return the smaller of the two finite durations
   */
  def min(other: FiniteDuration): FiniteDuration   = if (this < other) this else other
  /** Returns the larger of this finite duration and another finite duration.
   *
   *  @param other the finite duration to compare with
   *  @return the larger of the two finite durations
   */
  def max(other: FiniteDuration): FiniteDuration   = if (this > other) this else other

  // overloaded methods taking Long so that you can calculate while statically staying finite

  /** Returns the quotient of this duration and the given integer factor.
   *
   *  @param divisor the integer value to divide by
   *  @throws java.lang.ArithmeticException if the factor is 0
   */
  def /(divisor: Long): FiniteDuration = fromNanos(toNanos / divisor)

  /** Returns the product of this duration and the given integer factor.
   *
   *  @param factor the integer value to multiply by
   *  @throws IllegalArgumentException if the result would overflow the range of FiniteDuration
   */
  def *(factor: Long): FiniteDuration  = new FiniteDuration(safeMul(length, factor), unit)

  /*
   * This method avoids the use of Long division, which saves 95% of the time spent,
   * by checking that there are enough leading zeros so that the result has a chance
   * to fit into a Long again; the remaining edge cases are caught by using the sign
   * of the product for overflow detection.
   *
   * This method is not general purpose because it disallows the (otherwise legal)
   * case of Long.MinValue * 1, but that is okay for use in FiniteDuration, since
   * Long.MinValue is not a legal `length` anyway.
   */
  private def safeMul(_a: Long, _b: Long): Long = {
    val a = scala.math.abs(_a)
    val b = scala.math.abs(_b)
    import java.lang.Long.{ numberOfLeadingZeros => leading }
    if (leading(a) + leading(b) < 64) throw new IllegalArgumentException("multiplication overflow")
    val product = a * b
    if (product < 0) throw new IllegalArgumentException("multiplication overflow")
    if (a == _a ^ b == _b) -product else product
  }

  /** Returns the quotient of this duration and the given integer factor.
   *
   *  @param divisor the integer value to divide by
   *  @throws java.lang.ArithmeticException if the factor is 0
   */
  def div(divisor: Long): FiniteDuration = this / divisor

  /** Returns the product of this duration and the given integer factor.
   *
   *  @param factor the integer value to multiply by
   *  @throws IllegalArgumentException if the result would overflow the range of FiniteDuration
   */
  def mul(factor: Long): FiniteDuration  = this * factor

  /** Returns the negation of this finite duration.
   *
   *  @return the negated finite duration
   */
  def unary_- : FiniteDuration = Duration(-length, unit)

  /** Returns true since this is a finite duration.
   *
   *  @return true
   */
  final def isFinite = true

  /** Returns a finite duration with the coarsest possible time unit that can exactly represent this duration.
   *
   *  @return the duration with the coarsest possible time unit
   */
  final override def toCoarsest: FiniteDuration = {
    def loop(length: Long, unit: TimeUnit): FiniteDuration = {
      def coarserOrThis(coarser: TimeUnit, divider: Int): FiniteDuration =
        if (length % divider == 0) loop(length / divider, coarser)
        else if (unit == this.unit) this
        else FiniteDuration(length, unit)

      unit match {
        case DAYS => FiniteDuration(length, unit)
        case HOURS => coarserOrThis(DAYS, 24)
        case MINUTES => coarserOrThis(HOURS, 60)
        case SECONDS => coarserOrThis(MINUTES, 60)
        case MILLISECONDS => coarserOrThis(SECONDS, 1000)
        case MICROSECONDS => coarserOrThis(MILLISECONDS, 1000)
        case NANOSECONDS => coarserOrThis(MICROSECONDS, 1000)
      }
    }

    if (unit == DAYS || length == 0) this
    else loop(length, unit)
  }

  /** Compares this finite duration with another object for equality.
   *
   *  @param other the object to compare with
   *  @return true if the other object is a FiniteDuration with the same nanosecond length, false otherwise
   */
  override def equals(other: Any): Boolean = other match {
    case x: FiniteDuration => toNanos == x.toNanos
    case _                 => super.equals(other)
  }
  /** Returns a hash code for this finite duration based on its nanosecond length.
   *
   *  @return the hash code
   */
  override def hashCode(): Int = toNanos.toInt
}
