/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package scala
import annotation.showAsInfix

sealed trait Tuple extends Any
object Tuple

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple

object *: