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

@FunctionalInterface trait JFunction2$mcIDJ$sp extends Function2[Any, Any, Any] with Serializable {
  def apply$mcIDJ$sp(v1: Double, v2: Long): Int
  override def apply(v1: Any, v2: Any): Any = scala.runtime.BoxesRunTime.boxToInteger(apply$mcIDJ$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)))
}
