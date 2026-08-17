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
package util.hashing

import scala.language.`2.13`
import scala.annotation.implicitNotFound

/** `Hashing` is a trait whose instances each represent a strategy for hashing
  * instances of a type.
  *
  * `Hashing`'s companion object defines a default hashing strategy for all
  * objects - it calls their `##` method.
  *
  * Note: when using a custom `Hashing`, make sure to use it with the `Equiv`
  * such that if any two objects are equal, then their hash codes must be equal.
  */
@implicitNotFound(msg = "No implicit Hashing defined for ${T}.")
trait Hashing[T] extends Serializable {
  /** Computes the hash code for the given value.
   *
   *  @param x the value to hash
   *  @return the hash code of `x`
   */
  def hash(x: T): Int
}

object Hashing {
  /** A default hashing strategy that delegates to the `##` method of the value.
   *
   *  @tparam T the type of values to hash
   */
  final class Default[T] extends Hashing[T] {
    /** Computes the hash code by calling `##` on the given value.
     *
     *  @param x the value to hash
     */
    def hash(x: T) = x.##
  }

  /** Provides the default hashing strategy for type `T`.
   *
   *  @tparam T the type for which to provide the default hashing strategy
   *  @return a `Default` hashing instance for `T`
   */
  implicit def default[T]: Default[T] = new Default[T]

  /** Creates a `Hashing` instance from a function that computes hash codes.
   *
   *  @tparam T the type of values to hash
   *  @param f the function that computes hash codes for values of type `T`
   */
  def fromFunction[T](f: T => Int) = new Hashing[T] {
    def hash(x: T) = f(x)
  }
}
