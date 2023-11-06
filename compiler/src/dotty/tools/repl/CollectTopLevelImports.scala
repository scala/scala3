package dotty.tools.repl

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase

import scala.compiletime.uninitialized

/** A phase that collects user defined top level imports.
 *
 *  These imports must be collected as typed trees and therefore
 *  after Typer.
 */
class CollectTopLevelImports extends Phase {
  import tpd.*

  def phaseName: String = "collectTopLevelImports"

  private var myImports: List[Import] = uninitialized
  def imports: List[Import] = myImports

  def run(using Context): Unit = {
    def topLevelImports(tree: Tree) = {
      val PackageDef(_, _ :: TypeDef(_, rhs: Template) :: _) = tree: @unchecked
      rhs.body.collect { case tree: Import => tree }
    }

    val tree = ctx.compilationUnit.tpdTree
    myImports = topLevelImports(tree)
  }
}
