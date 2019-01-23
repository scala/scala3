/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala

/** A function with all parameters grouped in an array. */
trait FunctionXXL {

  def apply(xs: Array[Object]): Object

  override def toString() = "<functionXXL>"
}
