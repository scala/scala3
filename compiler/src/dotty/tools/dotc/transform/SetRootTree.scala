package dotty.tools.dotc.transform

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase

/** Set the `rootTreeOrProvider` property of class symbols. */
class SetRootTree extends Phase {

  override val phaseName: String = SetRootTree.name
  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.YretainTrees.value

  override def run(implicit ctx: Context): Unit = {
    val tree = ctx.compilationUnit.tpdTree
    traverser.traverse(tree)
  }

  private def traverser = new tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = {
      if (tree.isInstanceOf[tpd.TypeDef] && tree.symbol.isClass) {
        val sym = tree.symbol.asClass
        tpd.sliceTopLevel(ctx.compilationUnit.tpdTree, sym) match {
          case (pkg: tpd.PackageDef) :: Nil =>
            sym.rootTreeOrProvider = pkg
          case _ =>
            sym.rootTreeOrProvider = tree
        }
      }
      traverseChildren(tree)
    }
  }
}

object SetRootTree {
  val name: String = "SetRootTree"
}
