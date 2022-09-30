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

class CheckUnused extends Phase {
  import CheckUnused.UnusedDatas

  private val _key = Property.Key[UnusedDatas]

  override def phaseName: String = CheckUnused.phaseName

  override def description: String = CheckUnused.description

  override def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val data = UnusedDatas()
    val fresh = ctx.fresh.setProperty(_key, data)
    traverser.traverse(tree)(using fresh)
    reportUnusedImport(data.notFound)

  private def traverser = new TreeTraverser {
    import tpd._


    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match
      case imp@Import(_, sels) => sels.foreach { s =>
          if s.isGiven || s.isWildcard then // TODO: handle case
            ()
          else
            ctx.property(_key).foreach(_.defOrImported += imp)
            traverseChildren(tree)
        }

      case ident: Ident =>
        val id = ident.symbol.id
        ctx.property(_key).foreach(_.found += id)
        traverseChildren(tree)
      case sel: Select =>
        val id = sel.symbol.id
        ctx.property(_key).foreach(_.found += id)
        traverseChildren(tree)
      case _ => traverseChildren(tree)

  }

  private def reportUnusedImport(imports: Set[tpd.Import])(using Context) =
    if ctx.settings.WunusedHas.imports then
      imports.foreach { imp =>
        report.warning(i"unused import", imp.srcPos)
      }
}

object CheckUnused:
  val phaseName: String = "check unused"
  val description: String = "check for unused elements"

  private class UnusedDatas:
    import collection.{mutable, immutable}
    import mutable.Set

    val found: Set[Int] = Set()
    val defOrImported: Set[tpd.Import] = Set()

    def notFound(using Context): immutable.Set[tpd.Import] =
      defOrImported.toSet.filterNot { imp =>
        imp.selectors.exists{ sel =>
          val id = imp.expr.tpe.member(sel.name).symbol.id
          found(id)
        }
      }

  end UnusedDatas


