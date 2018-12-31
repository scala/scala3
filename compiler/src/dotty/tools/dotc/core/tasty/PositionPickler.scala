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
import util.Positions._
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
    var lastPos = Position(0, 0)
    def pickleDeltas(index: Int, pos: Position) = {
      val addrDelta = index - lastIndex
      val startDelta = pos.start - lastPos.start
      val endDelta = pos.end - lastPos.end
      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0, !pos.isSynthetic))
      if (startDelta != 0) buf.writeInt(startDelta)
      if (endDelta != 0) buf.writeInt(endDelta)
      if (!pos.isSynthetic) buf.writeInt(pos.pointDelta)
      lastIndex = index
      lastPos = pos

      pickledIndices += index
    }

    def pickleSource(source: AbstractFile): Unit = {
      buf.writeInt(SOURCE)
      buf.writeInt(pickler.nameBuffer.nameIndex(source.path.toTermName).index)
    }

    /** True if x's position shouldn't be reconstructed automatically from its initialPos
     */
    def alwaysNeedsPos(x: Positioned) = x match {
      case
          // initialPos is inaccurate for trees with lazy field
          _: WithLazyField[_]

          // A symbol is created before the corresponding tree is unpickled,
          // and its position cannot be changed afterwards.
          // so we cannot use the tree initialPos to set the symbol position.
          // Instead, we always pickle the position of definitions.
          | _: Trees.DefTree[_]

          // package defs might be split into several Tasty files
          | _: Trees.PackageDef[_] => true
      case _ => false
    }

    def traverse(x: Any, current: AbstractFile): Unit = x match {
      case x: untpd.Tree =>
        var sourceFile = current
        val pos = if (x.isInstanceOf[untpd.MemberDef]) x.pos else x.pos.toSynthetic
        if (pos.exists && (
          pos != x.initialPos.toSynthetic ||
          x.source.file != current ||
          alwaysNeedsPos(x))) {
          addrOfTree(x) match {
            case Some(addr) if !pickledIndices.contains(addr.index) =>
              //println(i"pickling $x with $pos at $addr")
              pickleDeltas(addr.index, pos)
              if (x.source.file != current) {
                pickleSource(x.source.file)
                sourceFile = x.source.file
              }
            case _ =>
              //println(i"no address for $x")
          }
        }
        //else if (x.pos.exists) println(i"skipping $x")
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
