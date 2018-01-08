package dotty.tools.dotc.core.tasty

/** Utils for String representation of TASTY */
object TastyString {

  /** Decode the TASTY String into TASTY bytes */
  def stringToTasty(str: String): Array[Byte] = {
    val bytes = new Array[Byte](str.length)
    for (i <- str.indices) bytes(i) = str.charAt(i).toByte
    bytes
  }

  /** Encode TASTY bytes into a TASTY String */
  def tastyToString(bytes: Array[Byte]): String = {
    val chars = new Array[Char](bytes.length)
    for (i <- bytes.indices) chars(i) = (bytes(i) & 0xff).toChar
    new String(chars)
  }

}
