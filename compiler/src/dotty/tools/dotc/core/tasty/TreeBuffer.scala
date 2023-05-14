package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.util.Util.dble
import dotty.tools.tasty.TastyBuffer
import TastyBuffer.{Addr, NoAddr, AddrWidth}

import util.Util.bestFit
import config.Printers.pickling
import ast.untpd.Tree
import java.util.Arrays

class TreeBuffer extends TastyBuffer(50000) {

  private inline val ItemsOverOffsets = 2
  private val initialOffsetSize = bytes.length / (AddrWidth * ItemsOverOffsets)
  private var offsets = new Array[Int](initialOffsetSize)
  private var isRelative = new Array[Boolean](initialOffsetSize)
  private var numOffsets = 0

  /** A map from trees to the address at which a tree is pickled. */
  private val treeAddrs = util.IntMap[Tree](initialCapacity = 8192)

  def registerTreeAddr(tree: Tree): Addr =
    val idx = treeAddrs(tree)
    if idx < 0 then
      treeAddrs(tree) = currentAddr.index
      currentAddr
    else
      Addr(idx)

  def addrOfTree(tree: Tree): Addr =
    val idx = treeAddrs(tree)
    if idx < 0 then NoAddr else Addr(idx)

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

  /** Reserve space for a reference, to be adjusted later */
  def reserveRef(relative: Boolean): Addr = {
    val addr = currentAddr
    keepOffset(relative)
    reserveAddr()
    addr
  }

  /** Write reference right adjusted into freshly reserved field. */
  def writeRef(target: Addr): Unit = {
    keepOffset(relative = false)
    fillAddr(reserveAddr(), target)
  }

  /** Fill previously reserved field with a reference */
  def fillRef(at: Addr, target: Addr, relative: Boolean): Unit = {
    val addr = if (relative) target.relativeTo(at) else target
    fillAddr(at, addr)
  }

  /** The amount by which the bytes at the given address are shifted under compression */
  def deltaAt(at: Addr, scratch: ScratchData): Int = {
    val idx = bestFit(offsets, numOffsets, at.index - 1)
    if (idx < 0) 0 else scratch.delta(idx)
  }

  /** The address to which `x` is translated under compression */
  def adjusted(x: Addr, scratch: ScratchData): Addr = x - deltaAt(x, scratch)

  /** Final assembly, involving the following steps:
   *   - compute deltas
   *   - adjust deltas until additional savings are < 1% of total
   *   - adjust offsets according to the adjusted deltas
   *   - shrink buffer, skipping zeroes.
   */
  def compactify(scratch: ScratchData): Unit =

    def reserve(arr: Array[Int]) =
      if arr.length < numOffsets then
        new Array[Int](numOffsets)
      else
        Arrays.fill(arr, 0, numOffsets, 0)
        arr

  /** Compute all shift-deltas */
    def computeDeltas() = {
      scratch.delta = reserve(scratch.delta)
      var lastDelta = 0
      var i = 0
      while (i < numOffsets) {
        val off = offset(i)
        val skippedOff = skipZeroes(off)
        val skippedCount = skippedOff.index - off.index
        assert(skippedCount < AddrWidth, s"unset field at position $off")
        lastDelta += skippedCount
        scratch.delta(i) = lastDelta
        i += 1
      }
    }

    /** The absolute or relative adjusted address at index `i` of `offsets` array*/
    def adjustedOffset(i: Int): Addr = {
      val at = offset(i)
      val original = getAddr(at)
      if (isRelative(i)) {
        val start = skipNat(at)
        val len1 = original + scratch.delta(i) - deltaAt(original + start.index, scratch)
        val len2 = adjusted(original + start.index, scratch) - adjusted(start, scratch).index
        assert(len1 == len2,
            s"adjusting offset #$i: $at, original = $original, len1 = $len1, len2 = $len2")
        len1
      }
      else adjusted(original, scratch)
    }

    /** Adjust all offsets according to previously computed deltas */
    def adjustOffsets(): Unit =
      var i = 0
      while i < numOffsets do
        val corrected = adjustedOffset(i)
        fillAddr(offset(i), corrected)
        i += 1

    /** Adjust deltas to also take account references that will shrink (and thereby
     *  generate additional zeroes that can be skipped) due to previously
     *  computed adjustments.
     */
    def adjustDeltas(): Int = {
      scratch.delta1 = reserve(scratch.delta1)
      var lastDelta = 0
      var i = 0
      while i < numOffsets do
        val corrected = adjustedOffset(i)
        lastDelta += AddrWidth - TastyBuffer.natSize(corrected.index)
        scratch.delta1(i) = lastDelta
        i += 1
      val saved =
        if (numOffsets == 0) 0
        else scratch.delta1(numOffsets - 1) - scratch.delta(numOffsets - 1)
      val tmp = scratch.delta
      scratch.delta = scratch.delta1
      scratch.delta1 = tmp
      saved
    }

    /** Compress pickle buffer, shifting bytes to close all skipped zeroes. */
    def compress(): Int = {
      var lastDelta = 0
      var start = 0
      var i = 0
      var wasted = 0
      def shift(end: Int) =
        System.arraycopy(bytes, start, bytes, start - lastDelta, end - start)
      while (i < numOffsets) {
        val next = offsets(i)
        shift(next)
        start = next + scratch.delta(i) - lastDelta
        val pastZeroes = skipZeroes(Addr(next)).index
        assert(pastZeroes >= start, s"something's wrong: eliminated non-zero")
        wasted += (pastZeroes - start)
        lastDelta = scratch.delta(i)
        i += 1
      }
      shift(length)
      length -= lastDelta
      wasted
    }

    def adjustTreeAddrs(): Unit =
      var i = 0
      while i < treeAddrs.size do
        treeAddrs.setValue(i, adjusted(Addr(treeAddrs.value(i)), scratch).index)
        i += 1

    val origLength = length
    computeDeltas()
    //println(s"offsets: ${offsets.take(numOffsets).deep}")
    //println(s"deltas: ${delta.take(numOffsets).deep}")
    var saved = 0
    while
      saved = adjustDeltas()
      pickling.println(s"adjusting deltas, saved = $saved")
      saved > 0 && length / saved < 100
    do ()
    adjustOffsets()
    adjustTreeAddrs()
    val wasted = compress()
    pickling.println(s"original length: $origLength, compressed to: $length, wasted: $wasted") // DEBUG, for now.
  end compactify
}
