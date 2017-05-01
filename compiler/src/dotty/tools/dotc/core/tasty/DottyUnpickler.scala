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
import dotty.tools.dotc.transform.linker.summaries.MethodSummary
import dotty.tools.dotc.transform.linker.summaries.TastySummaries
import scala.collection.mutable

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

  class SummariesTreeSectionUnpickler(symAtAddr: mutable.HashMap[Addr, Symbol], sectionName: String)
      extends TreeSectionUnpickler(posUnpickler = None) {
    override def unpickle(reader: TastyReader, tastyName: NameTable): SummariesTreeUnpickler = {
      new SummariesTreeUnpickler(symAtAddr, reader, tastyName, sectionName)
    }
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
  val treeUnpickler = unpickler.unpickle(new TreeSectionUnpickler(posUnpicklerOpt)).get

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(implicit ctx: Context): Unit =
    treeUnpickler.enterTopLevel(roots)

  /** The unpickled trees, and the source file they come from. */
  def body(implicit ctx: Context): List[Tree] = {
    treeUnpickler.unpickle()
  }

  def summaries(implicit ctx: Context): List[MethodSummary] = {
    val sectionName = TastySummaries.sectionName
    val tastySection = unpickler.unpickle(new SummariesTreeSectionUnpickler(treeUnpickler.symAtAddr, sectionName)).get
    val treeReader = tastySection.asInstanceOf[SummariesTreeUnpickler].getStartReader(ctx).get
    val unp = new TastyUnpickler.SectionUnpickler[List[MethodSummary]](sectionName) {
      def unpickle(reader: TastyReader, tastyName: NameTable): List[MethodSummary] =
        new TastySummaries.SummaryReader(treeReader, reader)(ctx).read()
    }
    unpickler.unpickle(unp).getOrElse(Nil)
  }
}
