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

  def header(addrDelta: Int, hasStartDelta: Boolean, hasEndDelta: Boolean) = {
    def toInt(b: Boolean) = if (b) 1 else 0
    (addrDelta << 2) | (toInt(hasStartDelta) << 1) | toInt(hasEndDelta)
  }

  def picklePositions(roots: List[Tree])(implicit ctx: Context) = {
    var lastIndex = 0
    var lastPos = Position(0, 0)
    def pickleDeltas(index: Int, pos: Position) = {
      val addrDelta = index - lastIndex
      val startDelta = pos.start - lastPos.start
      val endDelta = pos.end - lastPos.end
      buf.writeInt(header(addrDelta, startDelta != 0, endDelta != 0))
      if (startDelta != 0) buf.writeInt(startDelta)
      if (endDelta != 0) buf.writeInt(endDelta)
      lastIndex = index
      lastPos = pos
    }

    /** True if x's position cannot be reconstructed automatically from its initialPos
     */
    def needsPosition(x: Positioned) =
      x.pos.toSynthetic != x.initialPos.toSynthetic ||
      x.isInstanceOf[WithLazyField[_]] || // initialPos is inaccurate for trees with lazy fields
      x.isInstanceOf[Trees.PackageDef[_]] // package defs might be split into several Tasty files
    def traverse(x: Any): Unit = x match {
      case x: Tree @unchecked =>
        if (x.pos.exists && needsPosition(x)) {
          nextTreeAddr(x) match {
            case Some(addr) =>
              //println(i"pickling $x at $addr")
              pickleDeltas(addr.index, x.pos)
            case _ =>
              //println(i"no address for $x")
          }
        }
        //else if (x.pos.exists) println(i"skipping $x")
        x match {
          case x: MemberDef @unchecked => traverse(x.symbol.annotations.map(_.tree))
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
