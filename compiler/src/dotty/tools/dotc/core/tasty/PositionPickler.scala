package dotty.tools
package dotc
package core
package tasty

import ast._
import ast.Trees._
import ast.Trees.WithLazyField
import io.{AbstractFile, NoAbstractFile}
import core._
import Contexts._, Symbols._, Annotations._, Decorators._
import collection.mutable
import TastyBuffer._
import util.Spans._
import TastyFormat.SOURCE

class PositionPickler(pickler: TastyPickler, addrOfTree: untpd.Tree => Option[Addr]) {
  val buf: TastyBuffer = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)
  import ast.tpd._

  private val pickledIndices = new mutable.BitSet

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean): Int = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)
  }

  def picklePositions(roots: List[Tree])(implicit ctx: Context): Unit = {
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

      pickledIndices += index
    }

    def pickleSource(source: AbstractFile): Unit = {
      buf.writeInt(SOURCE)
      buf.writeInt(pickler.nameBuffer.nameIndex(source.path.toTermName).index)
    }

    /** True if x's position shouldn't be reconstructed automatically from its initial span
     */
    def alwaysNeedsPos(x: Positioned) = x match {
      case
          // initialSpan is inaccurate for trees with lazy field
          _: WithLazyField[_]

          // A symbol is created before the corresponding tree is unpickled,
          // and its position cannot be changed afterwards.
          // so we cannot use the tree initialSpan to set the symbol position.
          // Instead, we always pickle the position of definitions.
          | _: Trees.DefTree[_]

          // package defs might be split into several Tasty files
          | _: Trees.PackageDef[_] => true
      case _ => false
    }

    def traverse(x: Any, current: AbstractFile): Unit = x match {
      case x: untpd.Tree =>
        var sourceFile = current
        val span = if (x.isInstanceOf[untpd.MemberDef]) x.span else x.span.toSynthetic
        val sourceChange =
          x.source.file != x.elemsFile ||
          x.elemsFile == null && x.source.file != current
        if (span.exists && (
              span != x.initialSpan(ignoreTypeTrees = true).toSynthetic ||
              sourceChange ||
              alwaysNeedsPos(x))) {
          addrOfTree(x) match {
            case Some(addr) if !pickledIndices.contains(addr.index) || sourceChange =>
              // we currently do not share trees when unpickling, so if one path to a tree contains
              // a source change while another does not, we have to record the position of the tree twice
              // in order not to miss the source change. Test case is t3232a.scala.
              //println(i"pickling $x with $span at $addr")
              pickleDeltas(addr.index, span)
              if (x.source.file != current) {
                pickleSource(x.source.file)
                sourceFile = x.source.file
              }
            case _ =>
              //println(i"no address for $x")
          }
        }
        //else if (x.span.exists) println(i"skipping $x")
        x match {
          case x: untpd.MemberDef @unchecked =>
            for (ann <- x.symbol.annotations) traverse(ann.tree, sourceFile)
          case _ =>
        }
        traverse(x.productIterator, sourceFile)
      case xs: TraversableOnce[_] =>
        xs.foreach(traverse(_, current))
      case x: Annotation =>
        traverse(x.tree, current)
      case _ =>
    }
    for (root <- roots) {
      traverse(root, NoAbstractFile)
    }
  }
}
