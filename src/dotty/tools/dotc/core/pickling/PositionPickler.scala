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
  
  val noOp = () => ()
  
  def traverseAll(root: Tree, recorder: PositionRecorder)(implicit ctx: Context) = {
    import recorder.edge.{seq, offset}
    
    def elemsTraversal(xs: TraversableOnce[Any]): () => Unit = 
      (noOp /: xs) ((op, x) => () => seq(op, elemTraversal(x)))

    def elemTraversal(x: Any): () => Unit = () => x match {        
      case x: Tree @ unchecked =>
        if (x.pos.exists)
          for (addr <- addrOfTree(x))
            recorder.record(addr, offset(x.pos))
        
        val annotTraversal = x match {
          case x: MemberDef => elemsTraversal(x.symbol.annotations)
          case _ => noOp
        }
        val childrenTraversal = elemsTraversal(x.productIterator)
        seq(annotTraversal, childrenTraversal)
      case xs: List[_] =>
        elemsTraversal(xs)()
      case _ =>
        ()
    }
 
    elemTraversal(root)()
  }
  
  def picklePositions(root: Tree)(implicit ctx: Context) = {
    traverseAll(root, startPos)
    traverseAll(root, endPos)
  }
}