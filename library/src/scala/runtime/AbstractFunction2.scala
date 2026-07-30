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

/** A base class for two-parameter function implementations, allowing a function
 *  value to be defined by extending a class rather than the [[scala.Function2]]
 *  trait.
 *
 *  Both argument types and the result type are specialized over a selection of
 *  the primitive types, so a subclass can accept unboxed arguments and produce
 *  an unboxed result.
 */
abstract class AbstractFunction2[@specialized(Specializable.Args) -T1, @specialized(Specializable.Args) -T2, @specialized(Specializable.Return) +R] extends Function2[T1, T2, R] {

}
