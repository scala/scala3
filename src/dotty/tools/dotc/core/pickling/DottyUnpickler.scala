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

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
class DottyUnpickler(bytes: Array[Byte]) {
  import tpd._

  private val unpickler = new TastyUnpickler(bytes)
  private val treeUnpickler = unpickler.unpickle(new TreeSectionUnpickler).get

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(implicit ctx: Context): Unit =
    treeUnpickler.enterTopLevel(roots)

  /** The unpickled trees
   *  @param readPositions if true, trees get decorated with position information.
   */
  def body(readPositions: Boolean = false)(implicit ctx: Context): List[Tree] = {
    if (readPositions)
      for ((totalRange, positions) <- unpickler.unpickle(new PositionsSectionUnpickler()))
        treeUnpickler.usePositions(totalRange, positions)
    treeUnpickler.unpickle()
  }

  private class TreeSectionUnpickler extends SectionUnpickler[TreeUnpickler]("ASTs") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) =
      new TreeUnpickler(reader, tastyName)
  }

  private class PositionsSectionUnpickler extends SectionUnpickler[(Position, AddrToPosition)]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table) =
      new PositionUnpickler(reader).unpickle()
  }
}
