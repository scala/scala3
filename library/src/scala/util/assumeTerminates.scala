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

package scala.util

import scala.language.`2.13`

/** Wraps a block that will not be checked by the termination checker.
  */
object assumeTerminates:
  def apply[T](body: => T): T = body
