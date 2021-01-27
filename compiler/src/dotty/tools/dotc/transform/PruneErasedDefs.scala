package dotty.tools.dotc
package transform

import core._
import Contexts._
import DenotTransformers.SymTransformer
import Flags._
import SymDenotations._
import Symbols._
import Types._
import typer.RefChecks
import MegaPhase.MiniPhase
import StdNames.nme
import ast.tpd

/** This phase makes all erased term members of classes private so that they cannot
 *  conflict with non-erased members. This is needed so that subsequent phases like
 *  ResolveSuper that inspect class members work correctly.
 *  The phase also replaces all expressions that appear in an erased context by
 *  default values. This is necessary so that subsequent checking phases such
 *  as IsInstanceOfChecker don't give false negatives.
 *  Finally, the phase replaces `compiletime.uninitialized` on the right hand side
 *  of a mutable field definition by `_`. This avoids a "is declared erased, but is
 *  in fact used" error in Erasure and communicates to Constructors that the
 *  variable does not have an initializer.
 */
class PruneErasedDefs extends MiniPhase with SymTransformer { thisTransform =>
  import tpd._

  override def phaseName: String = PruneErasedDefs.name

  override def changesMembers: Boolean = true   // makes erased members private

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name, ExplicitOuter.name)

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if (sym.isEffectivelyErased && !sym.is(Private) && sym.owner.isClass)
      sym.copySymDenotation(initFlags = sym.flags | Private)
    else sym

  override def transformApply(tree: Apply)(using Context): Tree =
    if (tree.fun.tpe.widen.isErasedMethod)
      cpy.Apply(tree)(tree.fun, tree.args.map(trivialErasedTree))
    else tree

  override def transformValDef(tree: ValDef)(using Context): Tree =
    val sym = tree.symbol
    if sym.isEffectivelyErased && !tree.rhs.isEmpty then
      cpy.ValDef(tree)(rhs = trivialErasedTree(tree))
    else tree.rhs match
      case rhs: TypeApply
      if rhs.symbol == defn.Compiletime_uninitialized
         && sym.is(Mutable) && sym.owner.isClass =>
        cpy.ValDef(tree)(rhs = cpy.Ident(rhs)(nme.WILDCARD))
      case _ =>
        tree

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    if (tree.symbol.isEffectivelyErased && !tree.rhs.isEmpty)
      cpy.DefDef(tree)(rhs = trivialErasedTree(tree))
    else tree

  private def trivialErasedTree(tree: Tree)(using Context): Tree =
    tree.tpe.widenTermRefExpr.dealias.normalized match
      case ConstantType(c) => Literal(c)
      case _ => ref(defn.Predef_undefined)

}

object PruneErasedDefs {
  val name: String = "pruneErasedDefs"
}
