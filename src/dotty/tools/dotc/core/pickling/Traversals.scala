package dotty.tools.dotc
package core
package pickling

import util.Positions._
import ast.tpd._
import ast.Trees.Lazy
import core.Contexts._

object Traversals {
    
  def traverse(x: Any, leftOp: Tree => Unit, rightOp: Tree => Unit) = {
    
    def traverseElems(xs: TraversableOnce[Any]) = xs.foreach(traverseElem)
    
    def traverseElem(x: Any): Unit = x match {
      case x: Tree @ unchecked =>
/** TODO: pickle annotation positions 
        x match {
          case x: MemberDef => traverseElems(x.symbol.annotations)
          case _ =>
        } */
        leftOp(x)
        x match {
          case x: ValDef => 
            traverseElem(x.tpt)
            traverseUnlessLazy(x.unforced)
          case x: DefDef => 
            traverseElems(x.tparams)
            traverseElems(x.vparamss)
            traverseElem(x.tpt)
            traverseUnlessLazy(x.unforced)
          case x: Template =>
            traverseElem(x.constr)
            traverseElems(x.parents)
            traverseElem(x.self)
            traverseUnlessLazy(x.unforced)
          case _ =>
            traverseElems(x.productIterator)
        }
        rightOp(x)
      case xs: List[_] =>
        traverseElems(xs)
      case _ =>
    }
    
    def traverseUnlessLazy(x: Any) = x match {
      case x: Lazy[_] => 
      case _ => traverseElem(x)
    }
 
    traverseElem(x)
  }
}