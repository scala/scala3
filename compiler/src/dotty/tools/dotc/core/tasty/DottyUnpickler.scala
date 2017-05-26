package dotty.tools
package dotc
package core
package tasty

import Contexts._, SymDenotations._, Symbols._
import dotty.tools.dotc.ast.tpd
import TastyUnpickler._, TastyBuffer._
import util.Positions._
import util.{SourceFile, NoSource}
import Annotations.Annotation
import core.Mode
import classfile.ClassfileParser

object DottyUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  class TreeSectionUnpickler(posUnpickler: Option[PositionUnpickler])
  extends SectionUnpickler[TreeUnpickler]("ASTs") {
    def unpickle(reader: TastyReader, nameAtRef: NameTable) =
      new TreeUnpickler(reader, nameAtRef, posUnpickler)
  }

  class PositionsSectionUnpickler extends SectionUnpickler[PositionUnpickler]("Positions") {
    def unpickle(reader: TastyReader, nameAtRef: NameTable) =
      new PositionUnpickler(reader)
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 */
class DottyUnpickler(bytes: Array[Byte]) extends ClassfileParser.Embedded {
  import tpd._
  import DottyUnpickler._

  val unpickler = new TastyUnpickler(bytes)
  private val posUnpicklerOpt = unpickler.unpickle(new PositionsSectionUnpickler)
  private val treeUnpickler = unpickler.unpickle(new TreeSectionUnpickler(posUnpicklerOpt)).get

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(implicit ctx: Context): Unit =
    treeUnpickler.enterTopLevel(roots)

  /** Only used if `-Yretain-trees` is set. */
  private[this] var myBody: List[Tree] = _
  /** The unpickled trees, and the source file they come from. */
  def body(implicit ctx: Context): List[Tree] = {
    def computeBody() = treeUnpickler.unpickle()
    if (ctx.settings.YretainTrees.value) {
      if (myBody == null)
        myBody = computeBody()
      myBody
    } else computeBody()
  }
}
