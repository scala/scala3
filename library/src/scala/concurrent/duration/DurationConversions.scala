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

  /** Converts this duration in nanoseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R  = ev.convert(nanoseconds)
  /** Converts this duration in nanoseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def nanos[C](c: C)(implicit ev: Classifier[C]): ev.R        = nanoseconds(c)
  /** Converts this duration in nanoseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def nanosecond[C](c: C)(implicit ev: Classifier[C]): ev.R   = nanoseconds(c)
  /** Converts this duration in nanoseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def nano[C](c: C)(implicit ev: Classifier[C]): ev.R         = nanoseconds(c)

  /** Converts this duration in microseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def microseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(microseconds)
  /** Converts this duration in microseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def micros[C](c: C)(implicit ev: Classifier[C]): ev.R       = microseconds(c)
  /** Converts this duration in microseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def microsecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = microseconds(c)
  /** Converts this duration in microseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def micro[C](c: C)(implicit ev: Classifier[C]): ev.R        = microseconds(c)

  /** Converts this duration in milliseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def milliseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(milliseconds)
  /** Converts this duration in milliseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def millis[C](c: C)(implicit ev: Classifier[C]): ev.R       = milliseconds(c)
  /** Converts this duration in milliseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def millisecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = milliseconds(c)
  /** Converts this duration in milliseconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def milli[C](c: C)(implicit ev: Classifier[C]): ev.R        = milliseconds(c)

  /** Converts this duration in seconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def seconds[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(seconds)
  /** Converts this duration in seconds to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def second[C](c: C)(implicit ev: Classifier[C]): ev.R       = seconds(c)

  /** Converts this duration in minutes to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def minutes[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(minutes)
  /** Converts this duration in minutes to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def minute[C](c: C)(implicit ev: Classifier[C]): ev.R       = minutes(c)

  /** Converts this duration in hours to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def hours[C](c: C)(implicit ev: Classifier[C]): ev.R        = ev.convert(hours)
  /** Converts this duration in hours to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def hour[C](c: C)(implicit ev: Classifier[C]): ev.R         = hours(c)

  /** Converts this duration in days to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def days[C](c: C)(implicit ev: Classifier[C]): ev.R         = ev.convert(days)
  /** Converts this duration in days to the type selected by the given marker object.
   *
   *  @tparam C the singleton type of the marker object
   *  @param c the marker object selecting the conversion, either `span` or `fromNow`; only its type is used, never its value
   *  @param ev the `Classifier` that performs the conversion
   *  @return the converted result, of type `ev.R`
   */
  def day[C](c: C)(implicit ev: Classifier[C]): ev.R          = days(c)
}

/**
 * This object just holds some cogs which make the DSL machine work, not for direct consumption.
 */
object DurationConversions {
  /** A classifier that converts a duration to the result type selected by a marker object.
   *
   *  @tparam C the singleton type of the marker object that selects this classifier
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
