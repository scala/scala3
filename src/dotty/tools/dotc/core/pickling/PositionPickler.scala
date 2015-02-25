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
import Traversals._

class PositionPickler(pickler: TastyPickler, addrOfTree: Tree => Option[Addr]) {
  val buf = new PositionBuffer
  pickler.newSection("Positions", buf)
  import buf._
    
  private def record(tree: Tree, start: Boolean) =
    if (tree.pos.exists)
      for (addr <- addrOfTree(tree))
        buf.record(
          addr, 
          offset = if (start) tree.pos.start else tree.pos.end, 
          recordAlways = !start && tree.isInstanceOf[WithLazyField[_]])
    
  def picklePositions(roots: List[Tree])(implicit ctx: Context) =
    traverse(roots, record(_, true), record(_, false))
}