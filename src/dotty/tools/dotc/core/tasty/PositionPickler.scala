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

class PositionPickler(pickler: TastyPickler, addrsOfTree: tpd.Tree => List[Addr]) {
  val buf = new TastyBuffer(5000)
  pickler.newSection("Positions", buf)
  import buf._
  import ast.tpd._

  private val remainingAddrs = new java.util.IdentityHashMap[Tree, Iterator[Addr]]

  def nextTreeAddr(tree: Tree): Option[Addr] = remainingAddrs.get(tree) match {
    case null =>
      addrsOfTree(tree) match {
        case Nil =>
          None
        case addr :: Nil =>
          Some(addr)
        case addrs =>
          remainingAddrs.put(tree, addrs.iterator)
          nextTreeAddr(tree)
      }
    case it: Iterator[_] =>
      if (it.hasNext) Some(it.next) else None
  }

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
      case x: Trees.Tree[_] => x.isType   // types are unpickled as TypeTrees, so child positions are not available
      case _ => false
    }

    def traverse(x: Any): Unit = x match {
      case x: Tree @unchecked =>
        val pos = if (x.isInstanceOf[MemberDef]) x.pos else x.pos.toSynthetic
        if (pos.exists && (pos != x.initialPos.toSynthetic || alwaysNeedsPos(x))) {
          nextTreeAddr(x) match {
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
      case _ =>
    }
    traverse(roots)
  }
}
