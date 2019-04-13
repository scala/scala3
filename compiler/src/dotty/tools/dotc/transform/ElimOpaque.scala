package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.MegaPhase._
import Flags._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import Denotations.SingleDenotation
import SymDenotations.SymDenotation
import DenotTransformers._
import TypeUtils._

object ElimOpaque {
  val name: String = "elimOpaque"
}

/** Rewrites opaque type aliases to normal alias types */
class ElimOpaque extends MiniPhase with DenotTransformer {

  override def phaseName: String = ElimOpaque.name

  // Override checks need to take place before treating opaque types as aliases
  override def runsAfterGroupsOf: Set[String] = Set(typer.RefChecks.name)

  // base types of opaque aliases change
  override def changesBaseTypes = true

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
    if (ref.symbol.isOpaqueHelper)
      ref match {
        case sym: SymDenotation =>
          sym.copySymDenotation(
            info = TypeAlias(sym.opaqueAlias),
            initFlags = sym.flags &~ (Opaque | Deferred))
        case _ =>
          ref.derivedSingleDenotation(ref.symbol, TypeAlias(ref.info.extractOpaqueAlias))
    }
    else ref
}