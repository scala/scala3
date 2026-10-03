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

/** A base class for one-parameter function implementations, allowing a function
 *  value to be defined by extending a class rather than the [[scala.Function1]]
 *  trait.
 *
 *  Both the argument type and the result type are specialized over a selection of
 *  the primitive types, so a subclass can accept an unboxed argument and produce
 *  an unboxed result.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam R the return type of the function
 */
abstract class AbstractFunction1[@specialized(Specializable.Arg) -T1, @specialized(Specializable.Return) +R] extends Function1[T1, R] {

}
