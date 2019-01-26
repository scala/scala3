/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.quoted

/** Throwing this error in the implementation of a macro
 *  will result in a compilation error with the given message.
 */
class QuoteError(message: String) extends Throwable(message)

object QuoteError {
  /** Throws a QuoteError with the given message */
  def apply(message: => String): Nothing = throw new QuoteError(message)
}
