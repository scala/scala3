package dotty.tools.dotc.semanticdb.internal

import scala.language.unsafeNulls

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object MD5 {
  def compute(string: String): String = {
    compute(ByteBuffer.wrap(string.getBytes(StandardCharsets.UTF_8)))
  }
  def compute(buffer: ByteBuffer): String = {
    val md = MessageDigest.getInstance("MD5")
    md.update(buffer)
    bytesToHex(md.digest())
  }
  private val hexArray = "0123456789ABCDEF".toCharArray
  def bytesToHex(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    var j = 0
    while (j < bytes.length) {
      val v: Int = bytes(j) & 0xFF
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
      j += 1
    }
    new String(hexChars)
  }
}
