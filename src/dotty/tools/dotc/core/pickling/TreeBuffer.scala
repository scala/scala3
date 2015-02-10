package dotty.tools
package dotc
package core
package pickling

import util.Util.{bestFit, dble}
import TastyBuffer.{Addr, AddrWidth}

class TreeBuffer extends TastyBuffer(1000000) {

  private final val ItemsOverOffsets = 2
    
  private val initialOffsetSize = bytes.length / (AddrWidth * ItemsOverOffsets)
  private var offsets = new Array[Int](initialOffsetSize)
  private var isRelative = new Array[Boolean](initialOffsetSize)
  private var delta: Array[Int] = _
  private var numOffsets = 0
            
  private def offset(i: Int): Addr = Addr(offsets(i))

  private def keepOffset(relative: Boolean): Unit = {
    if (numOffsets == offsets.length) {
      offsets = dble(offsets)
      isRelative = dble(isRelative)
    }
    offsets(numOffsets) = length
    isRelative(numOffsets) = relative
    numOffsets += 1
  }
       
  def reserveRef(relative: Boolean): Addr = {
    val addr = currentAddr
    keepOffset(relative)
    reserveAddr()
    addr
  }

  def writeRef(target: Addr) = {
    keepOffset(relative = false)
    fillAddr(reserveAddr(), target)
  }
  
  def fillRef(at: Addr, target: Addr, relative: Boolean) = {
    val addr = if (relative) target.relativeTo(at) else target
    fillAddr(at, addr)
  }
  
  def deltaAt(at: Addr): Int = {
    val idx = bestFit(offsets, numOffsets, at.index - 1)
    if (idx < 0) 0 else delta(idx)
  }
    
  def adjusted(x: Addr): Addr = x - deltaAt(x)

  private def computeDeltas() = {
    delta = new Array[Int](numOffsets)
    var lastDelta = 0
    var i = 0
    while (i < numOffsets) {
      val off = offset(i)
      val skippedOff = skipZeroes(off)
      val skippedCount = skippedOff.index - off.index
      assert(skippedCount < AddrWidth, s"unset field at position $off")
      lastDelta += skippedCount
      delta(i) = lastDelta 
      i += 1
    }
  }
  
  private def adjustedOffset(i: Int): Addr = {
    val at = offset(i)
    val original = getAddr(at)
    if (isRelative(i)) {
      val start = skipNat(at)
      val len1 = original + delta(i) - deltaAt(original + start.index)
      val len2 = adjusted(original + start.index) - adjusted(start).index
      assert(len1 == len2, 
          s"adjusting offset #$i: $at, original = $original, len1 = $len1, len2 = $len2")
      len1
    } else adjusted(original)
  }
  
  private def adjustOffsets(): Unit = {
    for (i <- 0 until numOffsets) {
      val corrected = adjustedOffset(i)
      fillAddr(offset(i), corrected)
    }
  }
    
  private def adjustDeltas(): Int = {
    val delta1 = new Array[Int](delta.length)
    var lastDelta = 0
    var i = 0
    while (i < numOffsets) {
      val corrected = adjustedOffset(i)
      lastDelta += AddrWidth - TastyBuffer.natSize(corrected.index)
      delta1(i) = lastDelta
      i += 1
    }
    val saved = 
      if (numOffsets == 0) 0
      else delta1(numOffsets - 1) - delta(numOffsets - 1)
    delta = delta1
    saved
  }
    
  private def compress(): Int = {
    var lastDelta = 0
    var start = 0
    var i = 0
    var wasted = 0
    def shift(end: Int) =
      Array.copy(bytes, start, bytes, start - lastDelta, end - start)
    while (i < numOffsets) {
      val next = offsets(i)
      shift(next)
      start = next + delta(i) - lastDelta
      val pastZeroes = skipZeroes(Addr(next)).index
      assert(pastZeroes >= start, s"something's wrong: eliminated non-zero")
      wasted += (pastZeroes - start)
      lastDelta = delta(i)
      i += 1
    }
    shift(length)
    length -= lastDelta
    wasted
  }
  
  override def assemble(): Unit = {
    val origLength = length
    computeDeltas()
    //println(s"offsets: ${offsets.take(numOffsets).deep}")
    //println(s"deltas: ${delta.take(numOffsets).deep}")
    if (true) {
      var saved = 0
      do {
        saved = adjustDeltas()
        println(s"adjusting deltas, saved = $saved")
      } while (saved > 0 && length / saved < 100)
    }
    adjustOffsets()
    val wasted = compress()
    println(s"original length: $origLength, compressed to: $length, wasted: $wasted")
  }
}
