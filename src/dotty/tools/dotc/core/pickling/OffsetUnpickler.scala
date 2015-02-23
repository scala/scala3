package dotty.tools
package dotc
package core
package pickling

import java.io.IOException

import Contexts._
import ast.tpd

/** Unpickler for start or end offset of tree positions */
class OffsetUnpickler(reader: TastyReader, edge: Edge) {
  import reader._
  private val end = readEnd()
  private var lastOffset = 0
  private var nextOffset = 0
  private var nextAddr = 0

  private def next() = {
    lastOffset = nextOffset
    if (currentAddr != end) {
      nextOffset += readInt()
      nextAddr += readInt()
    }
  }  
      
  def traverse(trees: List[tpd.Tree])(implicit ctx: Context) = {
    next()
    edge.traverseAll(trees) { tree =>
      if (edge.offset(tree.pos) == nextAddr) next()
      tree.setPosUnchecked(edge.updateOffset(tree.pos, lastOffset))
    }
    assert(currentAddr == end)
  }
}