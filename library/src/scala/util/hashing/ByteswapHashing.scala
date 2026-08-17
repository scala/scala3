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

/** A fast multiplicative hash by Phil Bagwell.
 *
 *  @tparam T the type of values to be hashed
 */
final class ByteswapHashing[T] extends Hashing[T] {

  /** Computes the hash code of a value by applying the byteswap32 algorithm to its default hash code.
   *
   *  @param v the value to be hashed
   */
  def hash(v: T) = byteswap32(v.##)

}


object ByteswapHashing {

  private class Chained[T](h: Hashing[T]) extends Hashing[T] {
    /** Computes the hash code of a value by applying the byteswap32 algorithm to the result of another hashing function.
     *
     *  @param v the value to be hashed
     */
    def hash(v: T) = byteswap32(h.hash(v))
  }

  /** Composes another `Hashing` with the Byteswap hash.
   *
   *  @tparam T the type of values to be hashed
   *  @param h the hashing instance whose result is passed through byteswap hashing
   *  @return a `Hashing[T]` that applies `h` and then passes the result through the byteswap hash
   */
  def chain[T](h: Hashing[T]): Hashing[T] = new Chained(h)

}
