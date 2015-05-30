package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Flags._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Replace This references to module classes  in static methods by global identifiers to the
 *  corresponding modules.
 */
class ElimStaticThis extends MiniPhaseTransform {
  import ast.tpd._
  def phaseName: String = "elimStaticThis"

  override def transformThis(tree: This)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (!tree.symbol.is(Package) && ctx.owner.enclosingMethod.is(JavaStatic)) {
      assert(tree.symbol.is(ModuleClass))
      ref(tree.symbol.sourceModule)
    }
    else tree
}
