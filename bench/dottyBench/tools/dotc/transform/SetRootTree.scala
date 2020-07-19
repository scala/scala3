package dottyBench.tools.dotc.transform

import dottyBench.tools.dotc.CompilationUnit
import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Phases.Phase

/** Set the `rootTreeOrProvider` property of class symbols. */
class SetRootTree extends Phase {

  override val phaseName: String = SetRootTree.name
  override def isRunnable(using Ctx) =
    super.isRunnable && ctx.settings.YretainTrees.value

  // Check no needed. Does not transform trees
  override def isCheckable: Boolean = false

  override def run(using Ctx, CState): Unit = {
    val tree = ctx.compilationUnit.tpdTree
    traverser.traverse(tree)
  }

  private def traverser = new tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(using Ctx, CState): Unit =
      tree match {
        case pkg: tpd.PackageDef =>
          traverseChildren(pkg)
        case td: tpd.TypeDef =>
          if (td.symbol.isClass) {
            val sym = td.symbol.asClass
            tpd.sliceTopLevel(ctx.compilationUnit.tpdTree, sym) match {
              case (pkg: tpd.PackageDef) :: Nil =>
                sym.rootTreeOrProvider = pkg
              case _ =>
                sym.rootTreeOrProvider = td
            }
          }
        case _ =>
          ()
      }
  }
}

object SetRootTree {
  val name: String = "SetRootTree"
}
