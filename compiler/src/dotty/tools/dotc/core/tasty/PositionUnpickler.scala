package dotty.tools
package dotc
package core
package tasty

import scala.compiletime.uninitialized
import scala.collection.mutable

import dotty.tools.tasty.{TastyFormat, TastyBuffer, TastyReader}
import TastyFormat.SOURCE
import TastyBuffer.{Addr, NameRef}

import util.Spans.*
import Names.TermName

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader, nameAtRef: Int => TermName) {
  import reader.*

  private var myLineSizes: Array[Int] = uninitialized
  private var mySpans: mutable.Map[Addr, Span] = uninitialized
  private var mySourceNameRefs: mutable.Map[Addr, NameRef] = uninitialized
  private var isDefined = false

  def ensureDefined(): Unit = {
    if (!isDefined) {
      val lines = readNat()
      myLineSizes = new Array[Int](lines)
      var i = 0
      while i < lines do
        // This should be `readNat()` per the spec,
        // but previous compiler versions had a bug and could write `-1` as a Nat
        val lineSize = readLongNat()
        myLineSizes(i) += (if lineSize == 0xFFFFFFFFL then -1 else lineSize.toInt)
        i += 1

      mySpans = mutable.Map.empty[Addr, Span]
      mySourceNameRefs = mutable.Map.empty[Addr, NameRef]
      var curIndex = 0
      var curStart = 0
      var curEnd = 0
      while (!isAtEnd) {
        val header = readInt()
        if (header == SOURCE) {
          mySourceNameRefs(Addr(curIndex)) = readNameRef()
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
          mySpans(Addr(curIndex)) =
            if (hasPoint) Span(curStart, curEnd, curStart + readInt())
            else Span(curStart, curEnd)
        }
      }
      isDefined = true
    }
  }

  private[tasty] def spans: mutable.Map[Addr, Span] = {
    ensureDefined()
    mySpans
  }

  private[tasty] def sourceNameRefs: mutable.Map[Addr, NameRef] = {
    ensureDefined()
    mySourceNameRefs
  }

  // Can include -1 entries for "unknown", as previous compiler versions had a bug
  private[tasty] def lineSizes: Array[Int] = {
    ensureDefined()
    myLineSizes
  }

  def spanAt(addr: Addr): Span = spans.getOrElse(addr, NoSpan)
  def sourcePathAt(addr: Addr): String = sourceNameRefs.get(addr).fold("")(r => nameAtRef(r.index).toString)
}
