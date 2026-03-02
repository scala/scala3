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

package scala

import scala.language.`2.13`

/** A common supertype for companions of specializable types.
 *  Should not be extended in user code.
 */
trait Specializable

object Specializable {
  // No type parameter in @specialized annotation.
  trait SpecializedGroup

  // Smuggle a list of types by way of a tuple upon which Group is parameterized.
  class Group[T](value: T) extends SpecializedGroup

  final val Primitives:  Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)] = null.asInstanceOf[Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)]]
  final val Everything:  Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit, AnyRef)] = null.asInstanceOf[Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit, AnyRef)]]
  final val Bits32AndUp: Group[(Int, Long, Float, Double)] = null.asInstanceOf[Group[(Int, Long, Float, Double)]]
  final val Integral:    Group[(Byte, Short, Int, Long, Char)] = null.asInstanceOf[Group[(Byte, Short, Int, Long, Char)]]
  final val AllNumeric:  Group[(Byte, Short, Int, Long, Char, Float, Double)] = null.asInstanceOf[Group[(Byte, Short, Int, Long, Char, Float, Double)]]
  final val BestOfBreed: Group[(Int, Double, Boolean, Unit, AnyRef)] = null.asInstanceOf[Group[(Int, Double, Boolean, Unit, AnyRef)]]
  final val Unit:        Group[Tuple1[Unit]] = null.asInstanceOf[Group[Tuple1[Unit]]]

  final val Arg:         Group[(Int, Long, Float, Double)] = null.asInstanceOf[Group[(Int, Long, Float, Double)]]
  final val Args:        Group[(Int, Long, Double)] = null.asInstanceOf[Group[(Int, Long, Double)]]
  final val Return:      Group[(Int, Long, Float, Double, Boolean, Unit)] = null.asInstanceOf[Group[(Int, Long, Float, Double, Boolean, Unit)]]
}
