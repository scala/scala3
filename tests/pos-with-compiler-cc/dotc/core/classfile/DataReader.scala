package dotty.tools
package dotc
package core
package classfile

trait DataReader {

  def bp: Int
  def bp_=(i: Int): Unit

  def buf: Array[Byte]

  /** read a byte
    */
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte

  /** read some bytes
    */
  def nextBytes(len: Int): Array[Byte]

  /** read a character
    */
  def nextChar: Char

  /** read an integer
    */
  def nextInt: Int

  /** extract a character at position bp from buf
    */
  def getChar(mybp: Int): Char

  /** extract an integer at position bp from buf
    */
  def getByte(mybp: Int): Byte

  def getBytes(mybp: Int, bytes: Array[Byte]): Unit

  /** extract an integer at position bp from buf
    */
  def getInt(mybp: Int): Int

  /** extract a long integer at position bp from buf
    */
  def getLong(mybp: Int): Long

  /** extract a float at position bp from buf
    */
  def getFloat(mybp: Int): Float

  /** extract a double at position bp from buf
    */
  def getDouble(mybp: Int): Double

  def getUTF(mybp: Int, len: Int): String

  /** skip next 'n' bytes
    */
  def skip(n: Int): Unit
}
