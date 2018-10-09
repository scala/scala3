package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Symbols._
import typer.RefChecks
import MegaPhase.MiniPhase
import ast.tpd

/** This phase makes all erased term members of classes private so that they cannot
 *  conflict with non-erased members. This is needed so that subsequent phases like
 *  ResolveSuper that inspect class members work correctly.
 *  The phase also replaces all expressions that appear in an erased context by
 *  default values. This is necessary so that subsequent checking phases such
 *  as IsInstanceOfChecker don't give false negatives.
 */
class PruneErasedDefs extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._

  override def phaseName: String = PruneErasedDefs.name

  override def changesMembers: Boolean = true   // makes erased members private

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name, ExplicitOuter.name)

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.isEffectivelyErased && !sym.is(Private) && sym.owner.isClass)
      sym.copySymDenotation(initFlags = sym.flags | Private)
    else sym

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    if (tree.fun.tpe.widen.isErasedMethod)
      cpy.Apply(tree)(tree.fun, tree.args.map(arg => ref(defn.Predef_undefined)))
    else tree

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    if (tree.symbol.isEffectivelyErased && !tree.rhs.isEmpty)
      cpy.ValDef(tree)(rhs = ref(defn.Predef_undefined))
    else tree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree =
    if (tree.symbol.isEffectivelyErased && !tree.rhs.isEmpty)
      cpy.DefDef(tree)(rhs = ref(defn.Predef_undefined))
    else tree
}
object PruneErasedDefs {
  val name: String = "pruneErasedDefs"
}
