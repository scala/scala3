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
 *  @param bytes         bytearray from which we unpickle
 *  @param readPositions if true, trees get decorated with position information.
 */
class DottyUnpickler(bytes: Array[Byte], readPositions: Boolean = false)(implicit ctx: Context) {
  import tpd._

  val unpickler = new TastyUnpickler(bytes)

  def result: List[Tree] = {
    val trees = unpickler.unpickle(new TreeSectionUnpickler()).getOrElse(Nil)
    if (readPositions)
      unpickler.unpickle(new PositionsSectionUnpickler(trees))
    trees
  }

  class TreeSectionUnpickler()(implicit ctx: Context) extends SectionUnpickler[List[Tree]]("ASTs") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): List[Tree] =
      new TreeUnpickler(reader, tastyName, readPositions).unpickle()
  }
  
  class PositionsSectionUnpickler(trees: List[Tree])(implicit ctx: Context) extends SectionUnpickler[Unit]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): Unit =
      new PositionReader(reader).unpickle(trees)
  }
}
