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
    def split(sliceEnd: Int, acc: List[String]): List[String] = {
      if (sliceEnd == 0) acc
      else {
        val sliceStart = (sliceEnd - maxStringSize) max 0
        split(sliceStart, str.substring(sliceStart, sliceEnd) :: acc)
      }
    }
    split(str.length, Nil)
  }

  /** Decode the TASTY String into TASTY bytes */
  def unpickle(strings: Pickled): Array[Byte] = {
    val string = new StringBuilder
    strings.foreach(string.append)
    Base64.getDecoder().decode(string.result().getBytes(UTF_8))
  }

}
