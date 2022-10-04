package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.ast.untpd
/**
 * A compiler phase that checks for unused imports or definitions
 *
 * Basically, it gathers definition/imports and their usage. If a
 * definition/imports does not have any usage, then it is reported.
 */
class CheckUnused extends Phase {
  import CheckUnused.UnusedData

  private val _key = Property.Key[UnusedData]

  override def phaseName: String = CheckUnused.phaseName

  override def description: String = CheckUnused.description

  override def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val data = UnusedData()
    val fresh = ctx.fresh.setProperty(_key, data)
    traverser.traverse(tree)(using fresh)
    reportUnusedImport(data.notFound)

  /**
   * This traverse is the **main** component of this phase
   *
   * It traverse the tree the tree and gather the data in the
   * corresponding context property
   */
  private def traverser = new TreeTraverser {
    import tpd._

    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match
      case imp@Import(_, sels) => sels.foreach { s =>
          ctx.property(_key).foreach(_.registerImport(imp))
        }

      case ident: Ident =>
        val id = ident.symbol.id
        ctx.property(_key).foreach(_.registerUsed(id))
        traverseChildren(tree)
      case sel: Select =>
        val id = sel.symbol.id
        ctx.property(_key).foreach(_.registerUsed(id))
        traverseChildren(tree)
      case _ => traverseChildren(tree)

  }

  private def reportUnusedImport(sels: Set[ImportSelector])(using Context) =
    if ctx.settings.WunusedHas.imports then
      sels.foreach { s =>
        report.warning(i"unused import", s.srcPos)
      }
}

object CheckUnused:
  val phaseName: String = "check unused"
  val description: String = "check for unused elements"

  /**
   * A stateful class gathering the infos on :
   * - imports
   * - definitions
   * - usage
   */
  private class UnusedData:
    import collection.mutable.{Set => MutSet}

    private val used: MutSet[Int] = MutSet()
    private val imported: MutSet[tpd.Import] = MutSet()

    private def isImportExclusion(sel: ImportSelector): Boolean = sel.renamed match
      case ident@untpd.Ident(name) => name == StdNames.nme.WILDCARD
      case _ => false

    private def isUsedImportSelector(imp: tpd.Import, sel: ImportSelector)(using Context): Boolean =
      if sel.isGiven then
        true // TODO : handle this case
      else if sel.isWildcard then // NOT GIVEN
        imp.expr.tpe.allMembers.exists { m =>
          used(m.symbol.id)
        }
      else
        if isImportExclusion(sel) then
          true
        else
          used(imp.expr.tpe.member(sel.name).symbol.id)

    def notFound(using Context): Set[ImportSelector] =
      for {
        imp <- imported.toSet
        sel <- imp.selectors if !isUsedImportSelector(imp,sel)
      } yield sel

    /** Register the id of a found (used) symbol */
    def registerUsed(id: Int): Unit = used += id

    /** Register an import */
    def registerImport(imp: tpd.Import): Unit = imported += imp

  end UnusedData


