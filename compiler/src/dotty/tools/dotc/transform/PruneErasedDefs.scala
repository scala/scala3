package dotty.tools.dotc
package transform

import core.*
import Contexts.*
import DenotTransformers.SymTransformer
import Flags.*
import SymDenotations.*
import Symbols.*
import typer.RefChecks
import MegaPhase.MiniPhase

/** This phase makes all erased term members of classes private so that they cannot
 *  conflict with non-erased members. This is needed so that subsequent phases like
 *  ResolveSuper that inspect class members work correctly and so that we do not
 *  generate bridges for such members. See pos/i23451.scala for a test case.
 */
class PruneErasedDefs extends MiniPhase with SymTransformer:
  override def phaseName: String = PruneErasedDefs.name

  override def description: String = PruneErasedDefs.description

  override def changesMembers: Boolean = true   // makes erased members private

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name, ExplicitOuter.name)

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if !sym.is(Private) && sym.isEffectivelyErased && sym.isTerm && sym.owner.isClass
    then sym.copySymDenotation(initFlags = sym.flags | Private)
    else sym

object PruneErasedDefs:
  val name: String =  "pruneErasedDefs"
  val description: String = "drop erased definitions and simplify erased expressions"
end PruneErasedDefs