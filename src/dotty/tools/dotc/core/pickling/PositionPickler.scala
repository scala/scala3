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

  def picklePositions(roots: List[Tree], totalRange: Position)(implicit ctx: Context) = {
    var lastIndex = 0
    def record(tree: Tree, parentPos: Position): Unit =
      if (tree.pos.exists) {
        def msg = s"failure to pickle $tree at ${tree.pos}, parent = $parentPos"
        val endPos = tree.pos.end min parentPos.end
          // end positions can be larger than their parents
          // e.g. in the case of synthetic empty ranges, which are placed at the next token after
          // the current construct.
        val endDelta = endPos - parentPos.end
        val startPos =
          if (endDelta == 0) tree.pos.start max parentPos.start else tree.pos.start min endPos
          // Since end positions are corrected above, start positions have to follow suit.
        val startDelta = startPos - parentPos.start
        if (startDelta != 0 || endDelta != 0)
          for (addr <- addrOfTree(tree)) {
            buf.writeInt(addr.index - lastIndex)
            lastIndex = addr.index
            if (startDelta != 0) buf.writeInt(startDelta)
            if (endDelta != 0) {
              assert(endDelta < 0, msg)
              buf.writeInt(endDelta)
            } else
              assert(startDelta >= 0, msg)
          }
      }

    buf.writeNat(totalRange.end)
    traverse(roots, totalRange, record)
  }
}
