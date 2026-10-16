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

/** This class implements errors which are thrown whenever a
 *  field is used before it has been initialized.
 *
 *  Such runtime checks are not emitted by default.
 *  They can be enabled by the `-Xcheckinit` compiler option.
 *
 *  @param msg the error message describing which field was accessed before initialization
 */
final case class UninitializedFieldError(msg: String) extends RuntimeException(msg) {
  /** Creates an `UninitializedFieldError` whose message is the string
   *  representation of `obj`.
   *
   *  @param obj the value describing which field was accessed before initialization; a
   *             `null` value yields the message `"null"`
   */
  def this(obj: Any) = this("" + obj)
}

object UninitializedFieldError extends scala.runtime.AbstractFunction1[String, UninitializedFieldError]:
  /** Returns the name of this companion object, `"UninitializedFieldError"`, rather than
   *  the `<function1>` rendering inherited via [[scala.runtime.AbstractFunction1]] from
   *  [[scala.Function1]].
   */
  override def toString(): String = "UninitializedFieldError"
