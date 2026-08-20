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

package scala.collection
package generic

import scala.language.`2.13`
import language.experimental.captureChecking

/** Some bit operations.
  *
  *  See [[https://www.drmaciver.com/2008/08/unsigned-comparison-in-javascala/]] for
  *  an explanation of unsignedCompare.
  */
private[collection] object BitOperations {
  /** Provides bit manipulation operations for `Int` values.
    */
  trait Int {
    type Int = scala.Int
    /** Returns `true` if the bitwise AND of `i` and `mask` is zero.
      *
      *  @param i the integer value to check
      *  @param mask the bitmask to apply
      */
    def zero(i: Int, mask: Int)                 = (i & mask) == 0
    /** Returns the result of applying a bitmask to `i`.
      *
      *  @param i the integer value to mask
      *  @param mask the branch bit used to derive the mask applied to `i`
      */
    def mask(i: Int, mask: Int)                 = i & (complement(mask - 1) ^ mask)
    /** Returns `true` if the masked value of `key` matches `prefix`.
      *
      *  @param key the integer value to check
      *  @param prefix the expected prefix value
      *  @param m the bitmask to apply
      */
    def hasMatch(key: Int, prefix: Int, m: Int) = mask(key, m) == prefix
    /** Returns `true` if `i` is less than `j`, treating both as unsigned values.
      *
      *  @param i the first integer to compare
      *  @param j the second integer to compare
      */
    def unsignedCompare(i: Int, j: Int)         = (i < j) ^ (i < 0) ^ (j < 0)
    /** Returns `true` if `m1` is shorter than `m2` when treated as unsigned values.
      *
      *  @param m1 the first integer to compare
      *  @param m2 the second integer to compare
      */
    def shorter(m1: Int, m2: Int)               = unsignedCompare(m2, m1)
    /** Returns the bitwise complement of `i`.
      *
      *  @param i the integer value to complement
      */
    def complement(i: Int)                      = (-1) ^ i
    /** Returns a sequence of bits representing `num`, from most significant to least significant.
      *
      *  @param num the integer value to convert to bits
      */
    def bits(num: Int)                          = 31 to 0 by -1 map (i => (num >>> i & 1) != 0)
    /** Returns a string representation of the bits of `num`, separated by `sep`.
      *
      *  @param num the integer value to convert to a bit string
      *  @param sep the separator to use between bits
      */
    def bitString(num: Int, sep: String = "")   = bits(num) map (b => if (b) "1" else "0") mkString sep
    /** Returns the highest one bit of `j`.
      *
      *  @param j the integer value to process
      */
    def highestOneBit(j: Int)                   = java.lang.Integer.highestOneBit(j)
  }
  object Int extends Int

  /** Provides bit manipulation operations for `Long` values.
    */
  trait Long {
    type Long = scala.Long
    /** Returns `true` if the bitwise AND of `i` and `mask` is zero.
      *
      *  @param i the long value to check
      *  @param mask the bitmask to apply
      */
    def zero(i: Long, mask: Long)                  = (i & mask) == 0L
    /** Returns the result of applying a bitmask to `i`.
      *
      *  @param i the long value to mask
      *  @param mask the branch bit used to derive the mask applied to `i`
      */
    def mask(i: Long, mask: Long)                  = i & (complement(mask - 1) ^ mask)
    /** Returns `true` if the masked value of `key` matches `prefix`.
      *
      *  @param key the long value to check
      *  @param prefix the expected prefix value
      *  @param m the bitmask to apply
      */
    def hasMatch(key: Long, prefix: Long, m: Long) = mask(key, m) == prefix
    /** Returns `true` if `i` is less than `j`, treating both as unsigned values.
      *
      *  @param i the first long to compare
      *  @param j the second long to compare
      */
    def unsignedCompare(i: Long, j: Long)          = (i < j) ^ (i < 0L) ^ (j < 0L)
    /** Returns `true` if `m1` is shorter than `m2` when treated as unsigned values.
      *
      *  @param m1 the first long to compare
      *  @param m2 the second long to compare
      */
    def shorter(m1: Long, m2: Long)                = unsignedCompare(m2, m1)
    /** Returns the bitwise complement of `i`.
      *
      *  @param i the long value to complement
      */
    def complement(i: Long)                        = (-1L) ^ i
    /** Returns a sequence of bits representing `num`, from most significant to least significant.
      *
      *  @param num the long value to convert to bits
      */
    def bits(num: Long)                            = 63L to 0L by -1L map (i => (num >>> i & 1L) != 0L)
    /** Returns a string representation of the bits of `num`, separated by `sep`.
      *
      *  @param num the long value to convert to a bit string
      *  @param sep the separator to use between bits
      */
    def bitString(num: Long, sep: String = "")     = bits(num) map (b => if (b) "1" else "0") mkString sep
    /** Returns the highest one bit of `j`.
      *
      *  @param j the long value to process
      */
    def highestOneBit(j: Long)                     = java.lang.Long.highestOneBit(j)
  }
  object Long extends Long
}
