package dotty.tools
package dotc
package core
package tasty


import util.Positions._
import collection.mutable
import TastyBuffer.{Addr, NoAddr}

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader) {
  import reader._

  private[tasty] lazy val positions = {
    val positions = new mutable.HashMap[Addr, Position]
    var curIndex = 0
    var curStart = 0
    var curEnd = 0
    while (!isAtEnd) {
      val header = readInt()
      val addrDelta = header >> 3
      val hasStart = (header & 4) != 0
      val hasEnd = (header & 2) != 0
      val hasPoint = (header & 1) != 0
      curIndex += addrDelta
      assert(curIndex >= 0)
      if (hasStart) curStart += readInt()
      if (hasEnd) curEnd += readInt()
      positions(Addr(curIndex)) = 
        if (hasPoint) Position(curStart, curEnd, curStart + readInt())
        else Position(curStart, curEnd)
    }
    positions
  }

  def posAt(addr: Addr) = positions.getOrElse(addr, NoPosition)
}

