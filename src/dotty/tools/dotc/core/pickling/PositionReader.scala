package dotty.tools
package dotc
package core
package pickling

import java.io.IOException

import Contexts._
import ast.tpd
import ast.Trees.{Lazy,WithLazyField}
import TastyBuffer._
import Traversals._
import util.Positions._

/** Unpickler for start or end offset of tree positions */
class PositionReader(reader: TastyReader, startOffset: Int, initNextOffset: Int, initNextAddr: Int) {
  import reader._
  import tpd._
      
  def this(reader: TastyReader) = {
    this(reader, 0, 0, 0)
    next()
  }
  
  var lastOffset: Int = startOffset
  var nextOffset = initNextOffset
  var nextAddr = initNextAddr
  
  private def fork = 
    new PositionReader(subReader(currentAddr, endAddr), lastOffset, nextOffset, nextAddr)
  
  private def next() = {
    lastOffset = nextOffset
    if (!isAtEnd) {
      nextOffset += readInt()
      nextAddr += readInt()
    }
  }  

  def readStart(tree: Tree) = 
    if (tree.pos.exists) { // TODO assign positions of tree nodes with NoPosition afterwards based on the context.
      val addr = tree.pos.end
      if (addr == nextAddr) next()
      tree.setPosUnchecked(tree.pos.withStart(lastOffset))
    }
  
  def readEnd(tree: Tree) = {
    if (tree.pos.exists) {
      val addr = tree.pos.end
      tree match {
        case tree: WithLazyField[_] =>
          tree.unforced match {
            case rdr: TreeUnpickler#LazyReader[_] =>
              rdr.posReader = Some(fork)
              while (addr != nextAddr) next()
            case _ =>
          }
        case _ =>
      }
      if (addr == nextAddr) next()
      tree.setPosUnchecked(tree.pos.withEnd(lastOffset))
    }
  }
  
  def unpickle(x: Any) =
    traverse(x, readStart, readEnd)
}