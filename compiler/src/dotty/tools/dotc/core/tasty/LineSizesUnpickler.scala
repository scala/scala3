package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.{TastyFormat, TastyBuffer, TastyReader}
import TastyFormat.SOURCE
import TastyBuffer.{Addr, NameRef}

import util.Spans._
import collection.{mutable, Map}
import Names.TermName

/** Unpickler for tree positions */
class LineSizesUnpickler(reader: TastyReader) {
  import reader._

  private var mySizes: Array[Int] = _
  private var isDefined = false

  def ensureDefined(): Unit =
    if !isDefined then
      val sizeBuf = Array.newBuilder[Int]
      // Number of lines if all lines are 127 characters or less
      var lowSizeBound = endAddr.index - currentAddr.index
      sizeBuf.sizeHint(lowSizeBound)
      while !isAtEnd do sizeBuf += readInt()
      mySizes = sizeBuf.result()
      isDefined = true

  private[tasty] def sizes: Array[Int] =
    ensureDefined()
    mySizes

  private[tasty] def lineIndices: Array[Int] =
    val szs = sizes
    val indices = new Array[Int](sizes.length + 1)
    var i = 0
    val penultimate = szs.length - 1
    while i < penultimate do
      indices(i + 1) = indices(i) + szs(i) + 1 // `+1` for the '\n' at the end of the line
      i += 1
    indices(szs.length) = indices(penultimate) + szs(penultimate) // last line does not end with '\n'
    indices
}
