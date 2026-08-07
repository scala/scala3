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

/** A base class for four-parameter function implementations, allowing a function
 *  value to be defined by extending a class rather than the [[scala.Function4]]
 *  trait.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam R the return type of the function
 */
abstract class AbstractFunction4[-T1, -T2, -T3, -T4, +R] extends Function4[T1, T2, T3, T4, R] {

}
