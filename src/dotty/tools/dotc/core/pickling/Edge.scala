package dotty.tools.dotc
package core
package pickling

import util.Positions._
import ast.tpd.{Tree, MemberDef}
import core.Contexts._

abstract class Edge {
  
  def offset(pos: Position): Int
  def seq(op1: () => Unit, op2: () => Unit): Unit
  def updateOffset(pos: Position, off: Int): Position
  
  private val noOp = () => ()
  
  def traverseAll(roots: List[Tree])(op: Tree => Unit)(implicit ctx: Context) = {
    
    def elemsTraversal(xs: TraversableOnce[Any]): () => Unit = 
      (noOp /: xs) ((op, x) => () => seq(op, elemTraversal(x)))

    def elemTraversal(x: Any): () => Unit = () => x match {        
      case x: Tree @ unchecked =>
        op(x)
        
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
 
    elemsTraversal(roots)()
  }
}

object Edge {

  object left extends Edge {
    def offset(pos: Position): Int = pos.start
    def updateOffset(pos: Position, off: Int) = Position(off, pos.end, 0)
    def seq(op1: () => Unit, op2: () => Unit) = { op1(); op2() }
  }
  object right extends Edge {
    def offset(pos: Position): Int = pos.end
    def updateOffset(pos: Position, off: Int) = Position(pos.start, off, 0)
    def seq(op1: () => Unit, op2: () => Unit) = { op2(); op1() }
  }
}