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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

import scala.language.`2.13`

/** A function of 4 parameters.
 *
 */
trait Function4[-T1, -T2, -T3, -T4, +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4) == apply(x1, x2, x3, x4)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => R = {
    (x1: T1) => (x2: T2) => (x3: T3) => (x4: T4) => apply(x1, x2, x3, x4)
  }
  /** Creates a tupled version of this function: instead of 4 arguments,
   *  it accepts a single [[scala.Tuple4]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4)) == f(Tuple4(x1, x2, x3, x4)) == apply(x1, x2, x3, x4)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4)) => R = {
    case ((x1, x2, x3, x4)) => apply(x1, x2, x3, x4)
  }
  override def toString(): String = "<function4>"
}
