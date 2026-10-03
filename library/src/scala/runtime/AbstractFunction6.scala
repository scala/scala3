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

/** A base class for six-parameter function implementations, allowing a function
 *  value to be defined by extending a class rather than the [[scala.Function6]]
 *  trait.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam T5 the type of the 5th argument
 *  @tparam T6 the type of the 6th argument
 *  @tparam R the return type of the function
 */
abstract class AbstractFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends Function6[T1, T2, T3, T4, T5, T6, R] {

}
