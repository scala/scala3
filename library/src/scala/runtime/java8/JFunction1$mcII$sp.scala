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

package scala.runtime.java8

import scala.language.`2.13`

@FunctionalInterface trait JFunction1$mcII$sp extends Function1[Any, Any] with Serializable {
  def apply$mcII$sp(v1: Int): Int
  override def apply(t: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcII$sp(scala.runtime.BoxesRunTime.unboxToInt(t)))
}
