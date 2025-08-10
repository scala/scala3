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
package util.control

/** A trait for exceptions which, for efficiency reasons, do not
 *  fill in the stack trace.  Stack trace suppression can be disabled
 *  on a global basis via a system property wrapper in
 *  [[scala.sys.SystemProperties]].
 *
 *  @note Since JDK 1.7, a similar effect can be achieved with `class Ex extends Throwable(..., writableStackTrace = false)`
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable =
    if (NoStackTrace.noSuppression) super.fillInStackTrace()
    else this
}

object NoStackTrace {
  final def noSuppression = _noSuppression

  // two-stage init to make checkinit happy, since sys.SystemProperties.noTraceSupression.value calls back into NoStackTrace.noSuppression
  final private var _noSuppression = false
  // !!! Disabled in Scala.js because SystemProperties is not supported
  //_noSuppression = System.getProperty("scala.control.noTraceSuppression", "").equalsIgnoreCase("true")
}
