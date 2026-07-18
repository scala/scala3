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
  def this(obj: Any) = this("" + obj)
}

object UninitializedFieldError extends scala.runtime.AbstractFunction1[String, UninitializedFieldError]:
  override def toString(): String = "UninitializedFieldError"
