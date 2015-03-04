package dotty.tools
package dotc
package core
package pickling

import Contexts._, SymDenotations._
import dotty.tools.dotc.ast.tpd
import TastyUnpickler._, TastyBuffer._
import util.Positions._
import PositionUnpickler._

object DottyUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)
}

/** Unpickle symbol table information descending from a class and/or module root
 *  from an array of bytes.
 *  @param bytes         bytearray from which we unpickle
 *  @param roots         a set of SymDenotations that should be completed by unpickling
 *  @param readPositions if true, trees get decorated with position information.
 */
class DottyUnpickler(bytes: Array[Byte], roots: Set[SymDenotation], readPositions: Boolean = false)(implicit ctx: Context) {
  import tpd._

  val unpickler = new TastyUnpickler(bytes)
  
  def result: List[Tree] = {
    val (totalRange, positions) = 
      if (readPositions) 
        unpickler.unpickle(new PositionsSectionUnpickler())
          .getOrElse((NoPosition, null))
      else (NoPosition, null)
    unpickler.unpickle(new TreeSectionUnpickler(totalRange, positions))
      .getOrElse(Nil)
  }

  class TreeSectionUnpickler(totalRange: Position, positions: AddrToPosition)(implicit ctx: Context) 
      extends SectionUnpickler[List[Tree]]("ASTs") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): List[Tree] =
      new TreeUnpickler(reader, tastyName, roots, totalRange, positions).unpickle()
  }
  
  class PositionsSectionUnpickler()(implicit ctx: Context) extends SectionUnpickler[(Position, AddrToPosition)]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) =
      new PositionUnpickler(reader).unpickle()
  }
}
