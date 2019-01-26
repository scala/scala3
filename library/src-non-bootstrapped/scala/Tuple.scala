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

package scala
import annotation.showAsInfix

sealed trait Tuple extends Any
object Tuple

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple

object *: