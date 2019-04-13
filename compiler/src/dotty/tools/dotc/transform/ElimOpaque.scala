package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.MegaPhase._
import Flags._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import SymDenotations.SymDenotation
import DenotTransformers._
import TypeUtils._

object ElimOpaque {
  val name: String = "elimOpaque"
}

/** Rewrites opaque type aliases to normal alias types */
class ElimOpaque extends MiniPhase with SymTransformer {

  override def phaseName: String = ElimOpaque.name

  // Override checks need to take place before treating opaque types as aliases
  override def runsAfterGroupsOf: Set[String] = Set(typer.RefChecks.name)

  // base types of opaque aliases change
  override def changesBaseTypes = true

  override def transformNonSymInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    if (sym.isOpaqueHelper) TypeAlias(tp.extractOpaqueAlias) else tp

  def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.isOpaqueHelper) {
      sym.copySymDenotation(
        info = TypeAlias(sym.opaqueAlias),
        initFlags = sym.flags &~ (Opaque | Deferred))
    }
    else sym
}