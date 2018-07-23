package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Types._
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
class CutErasedDecls extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._

  override def phaseName = CutErasedDecls.name

  override def changesMembers = true   // makes erased members private

  override def runsAfterGroupsOf = Set(RefChecks.name)

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.is(Erased, butNot = Private) && sym.owner.isClass)
      sym.copySymDenotation(
        //name = UnlinkedErasedName.fresh(sym.name.asTermName),
        initFlags = sym.flags | Private)
    else sym

  override def transformApply(tree: Apply)(implicit ctx: Context) =
    if (tree.fun.tpe.widen.isErasedMethod)
      cpy.Apply(tree)(tree.fun, tree.args.map(arg => defaultValue(arg.tpe)))
    else tree

  override def transformValDef(tree: ValDef)(implicit ctx: Context) =
    if (tree.symbol.is(Erased) && !tree.rhs.isEmpty)
      cpy.ValDef(tree)(rhs = defaultValue(tree.rhs.tpe))
    else tree

  override def transformDefDef(tree: DefDef)(implicit ctx: Context) =
    if (tree.symbol.is(Erased) && !tree.rhs.isEmpty)
      cpy.DefDef(tree)(rhs = defaultValue(tree.rhs.tpe))
    else tree
}
object CutErasedDecls {
  val name = "cutErasedDecls"
}
