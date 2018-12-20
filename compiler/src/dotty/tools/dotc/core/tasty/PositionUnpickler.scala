package dotty.tools
package dotc
package core
package tasty

import util.Positions._
import collection.{mutable, Map}
import TastyBuffer.{Addr, NameRef}
import TastyFormat.SOURCE
import Names.TermName

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader, nameAtRef: NameRef => TermName) {
  import reader._

  private var myPositions: mutable.HashMap[Addr, Position] = _
  private var mySourcePaths: mutable.HashMap[Addr, String] = _
  private var isDefined = false

  def ensureDefined(): Unit =
    if (!isDefined) {
      myPositions = new mutable.HashMap[Addr, Position]
      mySourcePaths = new mutable.HashMap[Addr, String]
      var curIndex = 0
      var curStart = 0
      var curEnd = 0
      while (!isAtEnd) {
        val header = readInt()
        if (header == SOURCE) {
          val path = nameAtRef(readNameRef()).toString
          mySourcePaths(Addr(curIndex)) = path
        }
        else {
          val addrDelta = header >> 3
          val hasStart = (header & 4) != 0
          val hasEnd = (header & 2) != 0
          val hasPoint = (header & 1) != 0
          curIndex += addrDelta
          assert(curIndex >= 0)
          if (hasStart) curStart += readInt()
          if (hasEnd) curEnd += readInt()
          myPositions(Addr(curIndex)) =
            if (hasPoint) Position(curStart, curEnd, curStart + readInt())
            else Position(curStart, curEnd)
          }
      }
      isDefined = true
    }

  private[tasty] def positions: Map[Addr, Position] = {
    ensureDefined()
    myPositions
  }

  private[tasty] def sourcePaths: Map[Addr, String] = {
    ensureDefined()
    mySourcePaths
  }

  def posAt(addr: Addr): Position = positions.getOrElse(addr, NoPosition)
  def sourcePathAt(addr: Addr): String = sourcePaths.getOrElse(addr, "")
}

