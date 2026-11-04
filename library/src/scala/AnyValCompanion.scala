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

import language.experimental.captureChecking
import scala.language.`2.13`

/** A common supertype for companion classes of primitive types.
 *
 *  A common trait for /companion/ objects of primitive types comes handy
 *  when parameterizing code on types.
 */
private[scala] trait AnyValCompanion extends Specializable { }
