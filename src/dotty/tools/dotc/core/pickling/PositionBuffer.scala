package dotty.tools
package dotc
package core
package pickling

import TastyBuffer.Addr

class PositionBuffer extends TastyBuffer(100000) { thisBuffer =>
  
  private var lastOffset: Int = -1
  private var lastIndex: Int = 0
  
  def record(addr: Addr, offset: Int, recordAlways: Boolean): Unit =
    if (offset != lastOffset || recordAlways) {
      if (lastOffset < 0) lastOffset = 0
      writeInt(offset - lastOffset)
      writeInt(addr.index - lastIndex)
      lastOffset = offset
      lastIndex = addr.index
    }
}