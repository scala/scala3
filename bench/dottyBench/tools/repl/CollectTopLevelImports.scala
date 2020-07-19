package dottyBench.tools.repl

import dottyBench.tools.dotc.ast.Trees._
import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Phases.Phase

/** A phase that collects user defined top level imports.
 *
 *  These imports must be collected as typed trees and therefore
 *  after Typer.
 */
class CollectTopLevelImports extends Phase {
  import tpd._

  def phaseName: String = "collectTopLevelImports"

  private var myImports: List[Import] = _
  def imports: List[Import] = myImports

  def run(using Ctx, CState): Unit = {
    def topLevelImports(tree: Tree) = {
      val PackageDef(_, _ :: TypeDef(_, rhs: Template) :: _) = tree
      rhs.body.collect { case tree: Import => tree }
    }

    val tree = ctx.compilationUnit.tpdTree
    myImports = topLevelImports(tree)
  }
}
