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
      cs = Array.copyOf(cs, len * 2)
    cs(len) = ch
    len += 1

  def chars = cs
  def length = len
  def isEmpty: Boolean = len == 0
  def last: Char = cs(len - 1)
  def clear(): Unit = len = 0

  override def toString = String(cs, 0, len)


