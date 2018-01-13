package dotty.tools.dotc.core.tasty

import scala.runtime.quoted.Unpickler.Pickled

/** Utils for String representation of TASTY */
object TastyString {

  // Conservative encoding, each byte is encoded in a char
  // TODO improve encoding compression
  private final val maxStringSize = 65535 / 2

  /** Encode TASTY bytes into an Seq of String */
  def pickle(bytes: Array[Byte]): Pickled =
    bytes.sliding(maxStringSize, maxStringSize).map(bytesToString).toList

  /** Decode the TASTY String into TASTY bytes */
  def unpickle(strings: Pickled): Array[Byte] = {
    val bytes = new Array[Byte](strings.map(_.length).sum)
    var i = 0
    for (str <- strings; j <- str.indices) {
      bytes(i) = str.charAt(j).toByte
      i += 1
    }
    bytes
  }

  /** Encode bytes into a String */
  private def bytesToString(bytes: Array[Byte]): String = {
    assert(bytes.length <= maxStringSize)
    val chars = new Array[Char](bytes.length)
    for (i <- bytes.indices) chars(i) = (bytes(i) & 0xff).toChar
    new String(chars)
  }

}
