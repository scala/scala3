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
            
  private def offset(i: Int): Addr = new Addr(offsets(i))

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
    writeNat(target.index)
  }
  
  def fillRef(at: Addr, target: Addr, relative: Boolean) = {
    val addr = if (relative) target.relativeTo(at) else target
    fillAddr(at, addr)
  }
    
  def adjusted(x: Addr): Addr = {
    val idx = bestFit(offsets, numOffsets, x.index - 1)
    if (idx < 0) x else x - delta(idx)
  }

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
  
  private def adjustedOffset(at: Addr, isRelative: Boolean): Addr = {
    val original = getAddr(at)
    if (isRelative) {
      val start = skipNat(at).index
      adjusted(original + start) - start
    } else adjusted(original)
  }
  
  private def adjustOffsets(): Unit = {
    for (i <- 0 until numOffsets) {
      val off = offset(i)
      val original = getAddr(off)
      val corrected = adjustedOffset(off, isRelative(i))
      fillAddr(off, corrected)
    }
  }
    
  private def adjustDeltas(): Int = {
    val delta1 = new Array[Int](delta.length)
    var lastDelta = 0
    var i = 0
    while (i < numOffsets) {
      val corrected = adjustedOffset(offset(i), isRelative(i))
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
    while (i < numOffsets) {
      val next = offsets(i)
      Array.copy(bytes, start, bytes, start - lastDelta, next - start)
      start = next + delta(i) - lastDelta
      val pastZeroes = skipZeroes(new Addr(next)).index
      assert(pastZeroes >= start, s"something's wrong: eliminated non-zero")
      wasted += (pastZeroes - start)
      lastDelta = delta(i)
      i += 1
    }
    length -= lastDelta
    wasted
  }
  
  override def assemble(): Unit = {
    val origLength = length
    computeDeltas()
    adjustOffsets()
    if (false) {
      var saved = 0
      do saved = adjustDeltas()
      while (saved > 0 && length / saved < 100)
    }
    val wasted = compress()
    println(s"original length: $origLength, compressed to: $length, wasted: $wasted")
  }
}
