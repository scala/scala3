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

/** `AbstractFunction16` is a base class for sixteen-parameter function implementations.
 *
 *  Extending this class allows a function value to be defined by extending a class
 *  rather than the [[scala.Function16]] trait.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam T5 the type of the 5th argument
 *  @tparam T6 the type of the 6th argument
 *  @tparam T7 the type of the 7th argument
 *  @tparam T8 the type of the 8th argument
 *  @tparam T9 the type of the 9th argument
 *  @tparam T10 the type of the 10th argument
 *  @tparam T11 the type of the 11th argument
 *  @tparam T12 the type of the 12th argument
 *  @tparam T13 the type of the 13th argument
 *  @tparam T14 the type of the 14th argument
 *  @tparam T15 the type of the 15th argument
 *  @tparam T16 the type of the 16th argument
 *  @tparam R the return type of this function
 */
abstract class AbstractFunction16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] {

}
