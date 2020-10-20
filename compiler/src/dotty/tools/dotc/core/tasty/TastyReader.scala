package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.TastyBuffer._


class TastyReader(bytes: Array[Byte], start: Int, end: Int, base: Int = 0)
extends dotty.tools.tasty.TastyReader(bytes, start, end, base):

  def this(bytes: Array[Byte]) = this(bytes, 0, bytes.length)

  /** A new reader over the same array with the same address base, but with
   *  specified start and end positions
   */
  def subReader(start: Addr, end: Addr): TastyReader =
    new TastyReader(bytes, index(start), index(end), base)

  /** Perform `op` until `end` address is reached and collect results in a list. */
  def until[T](end: Addr)(op: => T): List[T] =
    val buf = List.Buffer[T]()
    while bp < index(end) do buf += op
    assert(bp == index(end))
    buf.tolist

  /** Perform `op` while condition `cond` holds and collect results in a list. */
  def collectWhile[T](cond: => Boolean)(op: => T): List[T] =
    val buf = List.Buffer[T]()
    while cond do buf += op
    buf.tolist
