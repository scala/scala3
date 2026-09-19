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
import scala.annotation.meta._

@field
/** Marks a mutable field as volatile, giving reads and writes of the field the JVM's volatile memory-visibility and ordering guarantees. */
final class volatile extends scala.annotation.StaticAnnotation
