package dotty.tools.repl

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.util
import util.Lst
import util.Lst.{NIL, +:, toLst}

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

  def run(using Context): Unit = {
    def topLevelImports(tree: Tree) = {
      val PackageDef(_, Lst(_, TypeDef(_, rhs: Template), _ : _*)) = tree
      rhs.body.collect{ case tree: Import => tree }.toScalaList
    }

    val tree = ctx.compilationUnit.tpdTree
    myImports = topLevelImports(tree)
  }
}
