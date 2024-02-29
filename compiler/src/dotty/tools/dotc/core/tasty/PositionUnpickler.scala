package dotty.tools
package dotc
package core
package tasty

import scala.compiletime.uninitialized

import dotty.tools.tasty.{TastyFormat, TastyBuffer, TastyReader}
import TastyFormat.SOURCE
import TastyBuffer.{Addr, NameRef}

import util.Spans.*
import Names.TermName

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader, nameAtRef: NameRef => TermName) {
  import reader.*

  private var myLineSizes: Array[Int] = uninitialized
  private var mySpans: util.HashMap[Addr, Span] = uninitialized
  private var mySourceNameRefs: util.HashMap[Addr, NameRef] = uninitialized
  private var isDefined = false

  def ensureDefined(): Unit = {
    if (!isDefined) {
      val lines = readNat()
      myLineSizes = new Array[Int](lines)
      var i = 0
      while i < lines do
        myLineSizes(i) += readNat()
        i += 1

      mySpans = util.HashMap[Addr, Span]()
      mySourceNameRefs = util.HashMap[Addr, NameRef]()
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

  private[tasty] def spans: util.ReadOnlyMap[Addr, Span] = {
    ensureDefined()
    mySpans
  }

  private[tasty] def sourceNameRefs: util.ReadOnlyMap[Addr, NameRef] = {
    ensureDefined()
    mySourceNameRefs
  }

  private[tasty] def lineSizes: Array[Int] = {
    ensureDefined()
    myLineSizes
  }

  def spanAt(addr: Addr): Span = spans.getOrElse(addr, NoSpan)
  def sourcePathAt(addr: Addr): String = sourceNameRefs.get(addr).fold("")(nameAtRef(_).toString)
}
