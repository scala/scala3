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

package scala.runtime

import scala.language.`2.13`
import scala.util.control.ControlThrowable

// remove Unit specialization when binary compatibility permits
// @annotation.nowarn("cat=lint-unit-specialization") TODO: Add warning back when specialization is implemented
class NonLocalReturnControl[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit) T](val key: AnyRef, val value: T) extends ControlThrowable {
  final override def fillInStackTrace(): Throwable = this
}
