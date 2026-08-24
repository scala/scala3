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
package collection

import scala.language.`2.13`
import language.experimental.captureChecking

protected[collection] object Hashing {

  /** Returns the hash code of the given key, computed with `##`.
   *
   *  @param key the value to hash
   *  @return `key.##`, which is `0` for `null` and consistent across boxed numeric
   *          types that compare equal
   */
  def elemHashCode(key: Any): Int = key.##

  /** Improves the distribution of a hash code by scrambling its bits with a sequence
   *  of shifts and xor/add steps, reducing collisions in hash tables that select
   *  buckets from only a subset of the bits.
   *
   *  @param hcode the hash code to improve
   *  @return the scrambled hash code
   */
  def improve(hcode: Int): Int = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  /** Returns the improved hash code of the given key.
   *
   *  @param key the value to hash
   *  @return the result of applying [[improve]] to the `##` hash code of `key`
   */
  def computeHash(key: Any): Int =
    improve(elemHashCode(key))

  /**
    * Utility method to keep a subset of all bits in a given bitmap
    *
    * Example
    *    bitmap (binary): 00000001000000010000000100000001
    *    keep (binary):                               1010
    *    result (binary): 00000001000000000000000100000000
    *
    * @param bitmap the bitmap
    * @param keep a bitmask containing which bits to keep
    * @return the original bitmap with all bits where keep is not 1 set to 0
    */
  def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

}
