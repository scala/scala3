package dotty.tools.dotc.quoted

import java.io.{ByteArrayInputStream, SequenceInputStream}
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.jdk.CollectionConverters.*

/** Utils for String representation of TASTY */
object TastyString {

  // Max size of a string literal in the bytecode
  private inline val maxStringSize = 65535

  /** Encode TASTY bytes into a List of String */
  def pickle(bytes: Array[Byte]): List[String] = {
    val str = Base64.getEncoder().encodeToString(bytes)
    str.toSeq.sliding(maxStringSize, maxStringSize).map(_.unwrap).toList
  }

  /** Decode the List of Strings into TASTY bytes */
  def unpickle(strings: List[String]): Array[Byte] = {
    val stream = new SequenceInputStream(
      strings.map(s => new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))).iterator.asJavaEnumeration
    )
    val result = Base64.getDecoder().wrap(stream)
    try result.readAllBytes()
    finally result.close()
  }

  /** Decode the Strings into TASTY bytes */
  def unpickle(string: String): Array[Byte] =
    Base64.getDecoder().decode(string)

}
