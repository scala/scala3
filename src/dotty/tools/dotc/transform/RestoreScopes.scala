package dotty.tools.dotc
package transform

import core._
import DenotTransformers.IdentityDenotTransformer
import Contexts.Context
import Symbols._
import Scopes._
import collection.mutable
import TreeTransforms.MiniPhaseTransform
import ast.Trees._
import TreeTransforms.TransformerInfo

/** The preceding lambda lift and flatten phases move symbols to different scopes
 *  and rename them. This miniphase cleans up afterwards and makes sure that all
 *  class scopes contain the symbols defined in them.
 */
class RestoreScopes extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._
  override def phaseName = "restoreScopes"

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo) = {
    val TypeDef(_, Template(constr, _, _, body)) = tree
    val restoredDecls = newScope
    for (stat <- constr :: body)
      if (stat.isInstanceOf[MemberDef] && stat.symbol.exists)
        restoredDecls.enter(stat.symbol)
    val cls = tree.symbol.asClass
    cls.owner.asClass.enter(cls) 
      // Enter class in enclosing package scope, in case it was an inner class before flatten.
      // For top-level classes this does nothing.
    val cinfo = cls.classInfo
    tree.symbol.copySymDenotation(
      info = cinfo.derivedClassInfo( // Dotty deviation: Cannot expand cinfo inline without a type error
        decls = restoredDecls: Scope)).installAfter(thisTransform)
    tree
  }
}
