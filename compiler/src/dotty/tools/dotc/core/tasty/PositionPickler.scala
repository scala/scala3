package dotty.tools
package dotc
package core
package tasty

import ast._
import ast.Trees._
import ast.Trees.WithLazyField
import TastyFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._
import collection.mutable
import TastyBuffer._
import util.Positions._

class PositionPickler(pickler: TastyPickler, addrOfTree: tpd.Tree => Option[Addr]) {
  val buf = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)
  import buf._
  import ast.tpd._

  private val remainingAddrs = new java.util.IdentityHashMap[Tree, Iterator[Addr]]

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean, hasPoint: Boolean) = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 3) | (toInt(hasStartDelta) << 2) | (toInt(hasEndDelta) << 1) | toInt(hasPoint)
  }

  def picklePositions(roots: List[Tree])(implicit ctx: Context) = {
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
    }

    /** True if x's position cannot be reconstructed automatically from its initialPos
     */
    def alwaysNeedsPos(x: Positioned) = x match {
      case _: WithLazyField[_]            // initialPos is inaccurate for trees with lazy field
         | _: Trees.PackageDef[_] => true // package defs might be split into several Tasty files
      case _ => false
    }

    def traverse(x: Any): Unit = x match {
      case x: Tree @unchecked =>
        val pos = if (x.isInstanceOf[MemberDef]) x.pos else x.pos.toSynthetic
        if (pos.exists && (pos != x.initialPos.toSynthetic || alwaysNeedsPos(x))) {
          addrOfTree(x) match {
            case Some(addr) =>
              //println(i"pickling $x with $pos at $addr")
              pickleDeltas(addr.index, pos)
            case _ =>
              //println(i"no address for $x")
          }
        }
        //else if (x.pos.exists) println(i"skipping $x")
        x match {
          case x: MemberDef @unchecked =>
            for (ann <- x.symbol.annotations) traverse(ann.tree)
          case _ =>
        }
        traverse(x.productIterator)
      case xs: TraversableOnce[_] =>
        xs.foreach(traverse)
      case x: Annotation =>
        traverse(x.tree)
      case _ =>
    }
    traverse(roots)
  }
}
