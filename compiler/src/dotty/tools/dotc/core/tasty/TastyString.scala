/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.core.tasty

import scala.runtime.quoted.Unpickler.Pickled

import java.io._
import java.util.Base64
import java.nio.charset.StandardCharsets.UTF_8

/** Utils for String representation of TASTY */
object TastyString {

  // Max size of a string literal in the bytecode
  private final val maxStringSize = 65535

  /** Encode TASTY bytes into an Seq of String */
  def pickle(bytes: Array[Byte]): Pickled = {
    val str = new String(Base64.getEncoder().encode(bytes), UTF_8)
    str.sliding(maxStringSize, maxStringSize).toList
  }

  /** Decode the TASTY String into TASTY bytes */
  def unpickle(strings: Pickled): Array[Byte] = {
    val string = new StringBuilder
    strings.foreach(string.append)
    Base64.getDecoder().decode(string.result().getBytes(UTF_8))
  }

}
