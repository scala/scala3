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

/** A base class for zero-parameter function implementations, allowing a function
 *  value to be defined by extending a class rather than the [[scala.Function0]]
 *  trait.
 *
 *  The result type is specialized over the primitive types, so a subclass can
 *  produce an unboxed result.
 *
 *  @tparam R the return type of the function
 */
abstract class AbstractFunction0[@specialized(Specializable.Primitives) +R] extends Function0[R] {

}
