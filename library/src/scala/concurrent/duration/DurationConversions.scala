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
import DurationConversions._

// Would be nice to limit the visibility of this trait a little bit,
// but it crashes scalac to do so.
/** Provides conversions from durations to various time units and classifiers. */
trait DurationConversions extends Any {
  /** Returns the duration in the specified time unit.
   *
   *  @param unit the time unit to convert to
   *  @return the duration in the specified time unit
   */
  protected def durationIn(unit: TimeUnit): FiniteDuration

  /** Returns the duration in nanoseconds. */
  def nanoseconds: FiniteDuration  = durationIn(NANOSECONDS)
  /** Returns the duration in nanoseconds. */
  def nanos: FiniteDuration        = nanoseconds
  /** Returns the duration in nanoseconds. */
  def nanosecond: FiniteDuration   = nanoseconds
  /** Returns the duration in nanoseconds. */
  def nano: FiniteDuration         = nanoseconds

  /** Returns the duration in microseconds. */
  def microseconds: FiniteDuration = durationIn(MICROSECONDS)
  /** Returns the duration in microseconds. */
  def micros: FiniteDuration       = microseconds
  /** Returns the duration in microseconds. */
  def microsecond: FiniteDuration  = microseconds
  /** Returns the duration in microseconds. */
  def micro: FiniteDuration        = microseconds

  /** Returns the duration in milliseconds. */
  def milliseconds: FiniteDuration = durationIn(MILLISECONDS)
  /** Returns the duration in milliseconds. */
  def millis: FiniteDuration       = milliseconds
  /** Returns the duration in milliseconds. */
  def millisecond: FiniteDuration  = milliseconds
  /** Returns the duration in milliseconds. */
  def milli: FiniteDuration        = milliseconds

  /** Returns the duration in seconds. */
  def seconds: FiniteDuration      = durationIn(SECONDS)
  /** Returns the duration in seconds. */
  def second: FiniteDuration       = seconds

  /** Returns the duration in minutes. */
  def minutes: FiniteDuration      = durationIn(MINUTES)
  /** Returns the duration in minutes. */
  def minute: FiniteDuration       = minutes

  /** Returns the duration in hours. */
  def hours: FiniteDuration        = durationIn(HOURS)
  /** Returns the duration in hours. */
  def hour: FiniteDuration         = hours

  /** Returns the duration in days. */
  def days: FiniteDuration         = durationIn(DAYS)
  /** Returns the duration in days. */
  def day: FiniteDuration          = days

  /** Converts the duration in nanoseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R  = ev.convert(nanoseconds)
  /** Converts the duration in nanoseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def nanos[C](c: C)(implicit ev: Classifier[C]): ev.R        = nanoseconds(c)
  /** Converts the duration in nanoseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def nanosecond[C](c: C)(implicit ev: Classifier[C]): ev.R   = nanoseconds(c)
  /** Converts the duration in nanoseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def nano[C](c: C)(implicit ev: Classifier[C]): ev.R         = nanoseconds(c)

  /** Converts the duration in microseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def microseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(microseconds)
  /** Converts the duration in microseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def micros[C](c: C)(implicit ev: Classifier[C]): ev.R       = microseconds(c)
  /** Converts the duration in microseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def microsecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = microseconds(c)
  /** Converts the duration in microseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def micro[C](c: C)(implicit ev: Classifier[C]): ev.R        = microseconds(c)

  /** Converts the duration in milliseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def milliseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(milliseconds)
  /** Converts the duration in milliseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def millis[C](c: C)(implicit ev: Classifier[C]): ev.R       = milliseconds(c)
  /** Converts the duration in milliseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def millisecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = milliseconds(c)
  /** Converts the duration in milliseconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def milli[C](c: C)(implicit ev: Classifier[C]): ev.R        = milliseconds(c)

  /** Converts the duration in seconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def seconds[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(seconds)
  /** Converts the duration in seconds using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def second[C](c: C)(implicit ev: Classifier[C]): ev.R       = seconds(c)

  /** Converts the duration in minutes using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def minutes[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(minutes)
  /** Converts the duration in minutes using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def minute[C](c: C)(implicit ev: Classifier[C]): ev.R       = minutes(c)

  /** Converts the duration in hours using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def hours[C](c: C)(implicit ev: Classifier[C]): ev.R        = ev.convert(hours)
  /** Converts the duration in hours using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def hour[C](c: C)(implicit ev: Classifier[C]): ev.R         = hours(c)

  /** Converts the duration in days using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def days[C](c: C)(implicit ev: Classifier[C]): ev.R         = ev.convert(days)
  /** Converts the duration in days using the given classifier.
   *
   *  @tparam C the type of the classifier
   *  @param c the classifier instance
   *  @param ev the implicit classifier instance
   *  @return the result of converting the duration using the classifier
   */
  def day[C](c: C)(implicit ev: Classifier[C]): ev.R          = days(c)
}

/**
 * This object just holds some cogs which make the DSL machine work, not for direct consumption.
 */
object DurationConversions {
  /** A classifier that converts a duration to a result type.
   *
   *  @tparam C the type of the classifier
   */
  trait Classifier[C] {
    type R
    /** Converts the given duration to the result type.
     *
     *  @param d the duration to convert
     *  @return the converted result
     */
    def convert(d: FiniteDuration): R
  }

  implicit object spanConvert extends Classifier[span.type] {
    type R = FiniteDuration
    /** Returns the duration unchanged.
     *
     *  @param d the duration to return
     *  @return the same duration
     */
    def convert(d: FiniteDuration): FiniteDuration = d
  }

  implicit object fromNowConvert extends Classifier[fromNow.type] {
    type R = Deadline
    /** Converts the duration to a deadline starting from now.
     *
     *  @param d the duration to add to the current time
     *  @return a deadline representing the current time plus the duration
     */
    def convert(d: FiniteDuration): Deadline = Deadline.now + d
  }

}
