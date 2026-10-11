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

/** Throwing this exception can be a temporary replacement for a method
 *  body that remains to be implemented. For instance, the exception is thrown by
 *  `Predef.???`.
 *
 *  @param msg the error message describing which implementation is missing
 */
final class NotImplementedError(msg: String) extends Error(msg) {
  def this() = this("an implementation is missing")
}
