/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala

/** A base trait of all enum classes */
trait Enum {

  /** A number uniquely identifying a case of an enum */
  def enumTag: Int
}
