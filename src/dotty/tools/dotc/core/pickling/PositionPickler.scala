package dotty.tools
package dotc
package core
package pickling

import ast.tpd._
import PickleFormat._
import core._
import Contexts._, Symbols._, Types._, Names._, Constants._, Decorators._, Annotations._
import collection.mutable
import TastyBuffer._

class PositionPickler(pickler: TastyPickler, addrOfTree: Tree => Option[Addr]) {
  val buf = new PositionBuffer
  pickler.newSection("Positions", buf)
  import buf._
    
  def picklePositions(roots: List[Tree])(implicit ctx: Context) = {
    def traverseWith(recorder: PositionRecorder) = 
      recorder.edge.traverseAll(roots) { tree =>
        if (tree.pos.exists)
          for (addr <- addrOfTree(tree))
            recorder.record(addr, recorder.edge.offset(tree.pos))
      }
    traverseWith(startPos)
    traverseWith(endPos)
  }
}