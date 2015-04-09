package dotty.tools
package dotc
package core
package pickling

import util.Positions._
import collection.mutable
import TastyBuffer.Addr

object PositionUnpickler {
  type AddrToPosition = mutable.HashMap[Addr, Position]
}

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader) {
  import PositionUnpickler._
  import reader._

  def unpickle(): (Position, AddrToPosition) = {
    val positions = new mutable.HashMap[Addr, Position] // Dotty deviation: Can't use new AddrToPosition here. TODO: fix this!
    val sourceLength = readNat()
    def readDelta() = if (isAtEnd) 0 else readInt()
    var curIndex: Addr = Addr(readDelta())
    while (!isAtEnd) {
      val delta1 = readDelta()
      val delta2 = readDelta()
      val (startDelta, endDelta, indexDelta) =
        if (delta2 <= 0) (delta1, -delta2, readDelta())
        else if (delta1 < 0) (0, -delta1, delta2)
        else (delta1, 0, delta2)
      positions(curIndex) = Position(startDelta, endDelta, startDelta)
        // make non-synthetic position; will be made synthetic by normalization.
      curIndex += indexDelta
    }
    (Position(0, sourceLength), positions)
  }
}
