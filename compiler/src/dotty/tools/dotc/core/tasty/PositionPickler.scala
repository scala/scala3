package dotty.tools
package dotc
package core
package tasty

import dotty.tools.tasty.TastyFormat.{SOURCE, PositionsSection}
import dotty.tools.tasty.TastyBuffer
import TastyBuffer.*

import ast.*
import Trees.WithLazyFields
import util.{SourceFile, NoSource}
import core.*
import Annotations.*, Decorators.*
import collection.mutable
import util.Spans.*
import reporting.Message

object PositionPickler:
  import ast.tpd.*

  // Note: This could be just TreeToAddr => Addr if functions are specialized to value classes.
  // We use a SAM type to avoid boxing of Addr
  @FunctionalInterface
  trait TreeToAddr:
    def apply(x: untpd.Tree): Addr

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean): Int =
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)

  def picklePositions(
      pickler: TastyPickler,
      addrOfTree: TreeToAddr,
      treeAnnots: untpd.MemberDef => List[tpd.Tree],
      typeAnnots: List[tpd.Tree],
      relativePathReference: String,
      source: SourceFile,
      roots: List[Tree],
      buf: TastyBuffer = new TastyBuffer(5000),
      pickledIndices: mutable.BitSet = new mutable.BitSet) =

    pickler.newSection(PositionsSection, buf)

    /** Pickle the number of lines followed by the size of each line.
     *  Manual `while` loop over the `Array[Char]` to avoid `ArrayOps.count`
     *  (which allocates a `Function1` and boxes each `Char` through
     *  `BoxesRunTime.equals`) and `ArrayOps.indexOf` (same boxing cost).
     *  Line break semantics are `'\n'`-only, matching
     *  `SourceFile.setLineIndicesFromLineSizes` which expects an `'\n'`
     *  terminator on every line except the last.
     */
    def pickleLinesSizes(): Unit = {
      val content = source.content()
      val len = content.length
      // First pass: count line breaks (primitive `if_icmpeq`, no boxing, no closure).
      var nlCount = 0
      var i = 0
      while i < len do
        if content(i) == '\n' then nlCount += 1
        i += 1
      buf.writeNat(nlCount + 1) // number of lines
      // Second pass: emit line sizes.
      var lineStart = 0
      i = 0
      while i < len do
        if content(i) == '\n' then
          buf.writeNat(i - lineStart) // size of this line (without the '\n')
          lineStart = i + 1
        i += 1
      buf.writeNat(len - lineStart) // size of the last (unterminated) line
    }
    pickleLinesSizes()

    var lastIndex = 0
    var lastSpan = Span(0, 0)
    def pickleDeltas(index: Int, span: Span) = {
      val addrDelta = index - lastIndex
      val startDelta = span.start - lastSpan.start
      val endDelta = span.end - lastSpan.end
      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0, !span.isSynthetic))
      if (startDelta != 0) buf.writeInt(startDelta)
      if (endDelta != 0) buf.writeInt(endDelta)
      if (!span.isSynthetic) buf.writeInt(span.pointDelta)
      lastIndex = index
      lastSpan = span

      pickledIndices.addOne(index)
        // Note `+=` boxes since it is a generic @inline function in `SetOps`
        // that forwards to the specialized `addOne` in `BitSet`. Since the
        // current backend does not implement `@inline` we are missing the
        // specialization.
    }

    def pickleSource(source: SourceFile): Unit = {
      buf.writeInt(SOURCE)
      val relativePath = SourceFile.relativePath(source, relativePathReference)
      buf.writeInt(pickler.nameBuffer.nameIndex(relativePath.toTermName).index)
    }

    /** True if x's position shouldn't be reconstructed automatically from its initial span
     */
    def alwaysNeedsPos(x: Positioned) = x match {
      case
          // initialSpan is inaccurate for trees with lazy field
          _: WithLazyFields

          // A symbol is created before the corresponding tree is unpickled,
          // and its position cannot be changed afterwards.
          // so we cannot use the tree initialSpan to set the symbol position.
          // Instead, we always pickle the position of definitions.
          | _: Trees.DefTree[?]

          // package defs might be split into several Tasty files
          | _: Trees.PackageDef[?]
          // holes can change source files when filled, which means
          // they might lose their position
          | _: Trees.Hole[?] => true
      case _ => false
    }

    def traverse(x: Any, current: SourceFile): Unit = x match {
      case x: untpd.Tree =>
        if (x.span.exists) {
          val addr = addrOfTree(x)
          if (addr != NoAddr) {
            if (x.source != current) {
              // we currently do not share trees when unpickling, so if one path to a tree contains
              // a source change while another does not, we have to record the position of the tree twice
              // in order not to miss the source change. Test case is t3232a.scala.
              pickleDeltas(addr.index, x.span)
              pickleSource(x.source)
            }
            else if (!pickledIndices.contains(addr.index) &&
                     (x.span.toSynthetic != x.envelope(x.source) || alwaysNeedsPos(x)))
              pickleDeltas(addr.index, x.span)
          }
        }
        x match
          case x: untpd.MemberDef => traverse(treeAnnots(x), x.source)
          case _ =>
        val limit = x.productArity
        var n = 0
        while (n < limit) {
          traverse(x.productElement(n), x.source)
          n += 1
        }
      case y :: ys =>
        traverse(y, current)
        traverse(ys, current)
      case _ =>
    }
    for (root <- roots)
      traverse(root, NoSource)

    for annotTree <- typeAnnots do
      traverse(annotTree, NoSource)
  end picklePositions
end PositionPickler

