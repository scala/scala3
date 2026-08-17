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

package scala.concurrent

import scala.language.`2.13`
import scala.language.implicitConversions

package object duration {
  /**
   * This object can be used as closing token if you prefer dot-less style but do not want
   * to enable language.postfixOps:
   *
   * ```scala sc:compile
   * import scala.concurrent.duration.*
   *
   * val duration = 2 seconds span
   * ```

   */
  object span

  /**
   * This object can be used as closing token for declaring a deadline at some future point
   * in time:
   *
   * ```scala sc:compile
   * import scala.concurrent.duration.*
   *
   * val deadline = 3 seconds fromNow
   * ```

   */
  object fromNow

  type TimeUnit          = java.util.concurrent.TimeUnit
  /** A time unit representing days. */
  final val DAYS         = java.util.concurrent.TimeUnit.DAYS
  /** A time unit representing hours. */
  final val HOURS        = java.util.concurrent.TimeUnit.HOURS
  /** A time unit representing microseconds. */
  final val MICROSECONDS = java.util.concurrent.TimeUnit.MICROSECONDS
  /** A time unit representing milliseconds. */
  final val MILLISECONDS = java.util.concurrent.TimeUnit.MILLISECONDS
  /** A time unit representing minutes. */
  final val MINUTES      = java.util.concurrent.TimeUnit.MINUTES
  /** A time unit representing nanoseconds. */
  final val NANOSECONDS  = java.util.concurrent.TimeUnit.NANOSECONDS
  /** A time unit representing seconds. */
  final val SECONDS      = java.util.concurrent.TimeUnit.SECONDS

  /** Converts a pair of Int and TimeUnit to a Duration.
   *
   *  @param p the pair of Int and TimeUnit to convert
   *  @return a Duration representing the given pair
   */
  implicit def pairIntToDuration(p: (Int, TimeUnit)): Duration         = Duration(p._1.toLong, p._2)
  /** Converts a pair of Long and TimeUnit to a FiniteDuration.
   *
   *  @param p the pair of Long and TimeUnit to convert
   *  @return a FiniteDuration representing the given pair
   */
  implicit def pairLongToDuration(p: (Long, TimeUnit)): FiniteDuration = Duration(p._1, p._2)
  /** Converts a Duration to a pair of Long and TimeUnit.
   *
   *  @param d the Duration to convert
   *  @return a pair of Long and TimeUnit representing the given Duration
   */
  implicit def durationToPair(d: Duration): (Long, TimeUnit)           = (d.length, d.unit)

  /** Provides duration conversion methods for Int values.
   *
   *  @param n the Int value to convert
   */
  implicit final class DurationInt(private val n: Int) extends AnyVal with DurationConversions {
    /** Creates a FiniteDuration from this Int and the given TimeUnit.
     *
     *  @param unit the TimeUnit to use for the duration
     *  @return a FiniteDuration representing this Int in the given TimeUnit
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  = Duration(n.toLong, unit)
  }

  /** Provides duration conversion methods for Long values.
   *
   *  @param n the Long value to convert
   */
  implicit final class DurationLong(private val n: Long) extends AnyVal with DurationConversions {
    /** Creates a FiniteDuration from this Long and the given TimeUnit.
     *
     *  @param unit the TimeUnit to use for the duration
     *  @return a FiniteDuration representing this Long in the given TimeUnit
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  = Duration(n, unit)
  }

  /** Provides duration conversion methods for Double values.
   *
   *  @param d the Double value to convert
   */
  implicit final class DurationDouble(private val d: Double) extends AnyVal with DurationConversions {
    /** Creates a FiniteDuration from this Double and the given TimeUnit.
     *
     *  @param unit the TimeUnit to use for the duration
     *  @return a FiniteDuration representing this Double in the given TimeUnit
     *  @throws IllegalArgumentException if the resulting Duration is not finite
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  =
      Duration(d, unit) match {
        case f: FiniteDuration => f
        case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
      }
  }

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  /** Provides multiplication methods for Int values with Duration.
   *
   *  @param i the Int value to multiply
   */
  implicit final class IntMult(private val i: Int) extends AnyVal {
    /** Multiplies this Int with a Duration.
     *
     *  @param d the Duration to multiply
     *  @return the product of this Int and the given Duration
     */
    def *(d: Duration): Duration             = d * i.toDouble
    /** Multiplies this Int with a FiniteDuration.
     *
     *  @param d the FiniteDuration to multiply
     *  @return the product of this Int and the given FiniteDuration
     */
    def *(d: FiniteDuration): FiniteDuration = d * i.toLong
  }

  /** Provides multiplication methods for Long values with Duration.
   *
   *  @param i the Long value to multiply
   */
  implicit final class LongMult(private val i: Long) extends AnyVal {
    /** Multiplies this Long with a Duration.
     *
     *  @param d the Duration to multiply
     *  @return the product of this Long and the given Duration
     */
    def *(d: Duration): Duration             = d * i.toDouble
    /** Multiplies this Long with a FiniteDuration.
     *
     *  @param d the FiniteDuration to multiply
     *  @return the product of this Long and the given FiniteDuration
     */
    def *(d: FiniteDuration): FiniteDuration = d * i.toLong
  }

  /** Provides multiplication methods for Double values with Duration.
   *
   *  @param f the Double value to multiply
   */
  implicit final class DoubleMult(private val f: Double) extends AnyVal {
    /** Multiplies this Double with a Duration.
     *
     *  @param d the Duration to multiply
     *  @return the product of this Double and the given Duration
     */
    def *(d: Duration): Duration             = d * f.toDouble
  }
}
