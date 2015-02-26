package dotty.tools
package dotc
package core
package pickling

import ast.tpd._
import ast.Trees.WithLazyField
import PickleFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._
import collection.mutable
import TastyBuffer._
import util.Positions._

object PositionPickler {
  
  trait DeferredPosition {
    var parentPos: Position = NoPosition
  }

  def traverse(x: Any, parentPos: Position, op: (Tree, Position) => Unit)(implicit ctx: Context): Unit = 
    if (parentPos.exists) 
      x match {
        case x: Tree @unchecked =>
          op(x, parentPos)
          x match {
            case x: MemberDef @unchecked => traverse(x.symbol.annotations, x.pos, op)
            case _ =>
          }
          traverse(x.productIterator, x.pos, op)
        case x: DeferredPosition =>
          x.parentPos = parentPos
        case xs: TraversableOnce[_] =>
          xs.foreach(traverse(_, parentPos, op))
        case _ =>
      }  
}
import PositionPickler._

class PositionPickler(pickler: TastyPickler, addrOfTree: Tree => Option[Addr]) {
  val buf = new TastyBuffer(100000)
  pickler.newSection("Positions", buf)
  import buf._
  
  private def record(tree: Tree, parentPos: Position): Unit = {
    assert(tree.pos.exists)
    val startDelta = tree.pos.start - parentPos.start
    val endDelta = tree.pos.end - parentPos.end
    if (startDelta != 0 || endDelta != 0)
      for (addr <- addrOfTree(tree)) {
        buf.writeInt(addr.index)
        if (startDelta != 0) buf.writeInt(startDelta)
        if (endDelta != 0) {
          assert(endDelta < 0)
          buf.writeInt(endDelta)
        } else assert(startDelta >= 0)
      }
    
  }
    
  def picklePositions(roots: List[Tree], totalRange: Position)(implicit ctx: Context) = {
    buf.writeNat(totalRange.end)
    traverse(roots, totalRange, record)
  }
}