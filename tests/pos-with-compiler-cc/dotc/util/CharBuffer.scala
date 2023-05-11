package dotty.tools
package dotc
package util

/** A character buffer that exposes the internal array for reading.
 *  That way we can avoid copying when converting to names.
 */
class CharBuffer(initialSize: Int = 1024):
  private var cs: Array[Char] = new Array[Char](initialSize)
  private var len: Int = 0

  def append(ch: Char): Unit =
    if len == cs.length then
      val cs1 = new Array[Char](len * 2)
      Array.copy(cs, 0, cs1, 0, len)
      cs = cs1
    cs(len) = ch
    len += 1

  def chars = cs
  def length = len
  def isEmpty: Boolean = len == 0
  def last: Char = cs(len - 1)
  def clear(): Unit = len = 0

  override def toString = String(cs, 0, len)


