package dotty.tools.dotc.transform

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/** Set the `rootTreeOrProvider` property of class symbols. */
class SetRootTree extends MiniPhase {

  override val phaseName: String = SetRootTree.name
  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.YretainTrees.value

  def runOn(unit: CompilationUnit)(implicit ctx: Context) = {
    val runCtx = ctx.fresh.setCompilationUnit(unit)
    traverser.traverse(unit.tpdTree)(runCtx)
  }

  override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context): tpd.Tree = {
    if (tree.symbol.isClass) {
      val sym = tree.symbol.asClass
      tpd.sliceTopLevel(ctx.compilationUnit.tpdTree, sym) match {
        case (pkg: tpd.PackageDef) :: Nil =>
          sym.rootTreeOrProvider = pkg
        case _ =>
          sym.rootTreeOrProvider = tree
      }
    }
    tree
  }

  private def traverser = new tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = tree match {
      case typeDef: tpd.TypeDef => transformTypeDef(typeDef)
      case other => traverseChildren(other)
    }
  }


}

object SetRootTree {
  val name: String = "SetRootTree"
}
