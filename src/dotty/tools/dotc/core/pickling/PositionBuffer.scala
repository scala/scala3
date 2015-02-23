package dotty.tools
package dotc
package core
package pickling

import TastyBuffer.Addr

class PositionBuffer extends TastyBuffer(100000) { thisBuffer =>
  
  class PositionRecorder(val edge: Edge) extends TastyBuffer(thisBuffer.bytes.size / 2) {
    private var lastOffset: Int = -1
    private var lastIndex: Int = 0
    def record(addr: Addr, offset: Int): Unit = 
      if (offset != lastOffset) {
        if (lastOffset < 0) lastOffset = 0
        writeInt(offset - lastOffset)
        writeInt(addr.index - lastIndex)
        lastOffset = offset
        lastIndex = addr.index
      }
  }
  
  val startPos = new PositionRecorder(Edge.left)
  val endPos = new PositionRecorder(Edge.right)

  /** Final assembly: copy startPos and endPos into own bytes */
  override def assemble(): Unit = {
    writeNat(startPos.length)
    writeBytes(startPos.bytes, startPos.length)
    writeNat(endPos.length)
    writeBytes(endPos.bytes, endPos.length)
  }
}