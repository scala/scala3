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
    
  def traverseAll(root: Tree, recorder: PositionRecorder)(implicit ctx: Context) = 
    recorder.edge.traverseAll(root) { tree =>
      if (tree.pos.exists)
        for (addr <- addrOfTree(tree))
          recorder.record(addr, recorder.edge.offset(tree.pos))
    }
  
  def picklePositions(root: Tree)(implicit ctx: Context) = {
    traverseAll(root, startPos)
    traverseAll(root, endPos)
  }
}