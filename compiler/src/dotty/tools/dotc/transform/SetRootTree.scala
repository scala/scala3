package dotty.tools.dotc.transform

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase

/** Set the `rootTreeOrProvider` property of class symbols. */
class SetRootTree extends Phase {

  override val phaseName: String = SetRootTree.name

  override val description: String = SetRootTree.description

  override def isRunnable(using Context) =
    super.isRunnable && ctx.settings.YretainTrees.value

  // Check no needed. Does not transform trees
  override def isCheckable: Boolean = false

  override def run(using Context): Unit = {
    val tree = ctx.compilationUnit.tpdTree
    traverser.traverse(tree)
  }

  private def traverser = new tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(using Context): Unit =
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
  val description: String = "set the rootTreeOrProvider on class symbols"
}
