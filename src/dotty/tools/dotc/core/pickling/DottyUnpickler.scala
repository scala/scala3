package dotty.tools
package dotc
package core
package pickling

import Contexts._, SymDenotations._
import dotty.tools.dotc.ast.tpd
import TastyUnpickler._, TastyBuffer._

object DottyUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)
}

/** Unpickle symbol table information descending from a class and/or module root
 *  from an array of bytes.
 *  @param bytes      bytearray from which we unpickle
 *  @param classroot  the top-level class which is unpickled, or NoSymbol if inapplicable
 *  @param moduleroot the top-level module class which is unpickled, or NoSymbol if inapplicable
 *  @param filename   filename associated with bytearray, only used for error messages
 */
class DottyUnpickler(bytes: Array[Byte], classRoot: ClassDenotation, moduleClassRoot: ClassDenotation, readPositions: Boolean = false)(implicit ctx: Context) {
  import tpd._

  val moduleRoot = moduleClassRoot.sourceModule.denot
  assert(moduleRoot.isTerm)

  val unpickler = new TastyUnpickler(bytes)

  def result: List[Tree] =
    unpickler.unpickle(new TreeSectionUnpickler()).getOrElse(Nil)
    
  if (readPositions)
    unpickler.unpickle(new PositionsSectionUnpickler())

  class TreeSectionUnpickler()(implicit ctx: Context) extends SectionUnpickler[List[Tree]]("ASTs") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): List[Tree] =
      new TreesUnpickler(reader, tastyName, Set(classRoot, moduleClassRoot, moduleRoot), readPositions)
        .unpickle()
  }
  
  class PositionsSectionUnpickler()(implicit ctx: Context) extends SectionUnpickler[Unit]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): Unit = {
      new OffsetUnpickler(reader, Edge.left).traverse(result)
      new OffsetUnpickler(reader, Edge.right).traverse(result)
    }
  }
}
