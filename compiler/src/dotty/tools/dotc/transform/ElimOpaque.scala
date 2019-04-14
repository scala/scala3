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

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val sym = ref.symbol
    ref match {
      case ref: SymDenotation if sym.isOpaqueHelper =>
        ref.copySymDenotation(
          info = TypeAlias(ref.opaqueAlias),
          initFlags = ref.flags &~ (Opaque | Deferred))
      case ref: SymDenotation if sym.isOpaqueCompanion =>
        val cinfo = sym.asClass.classInfo
        val RefinedType(sourceRef, _, _) = cinfo.selfInfo
        val ref1 = ref.copySymDenotation(
          info = cinfo.derivedClassInfo(selfInfo = sourceRef),
          initFlags = ref.flags &~ Opaque)
        ref1.registeredCompanion = NoSymbol
        ref1
      case _ if sym.isOpaqueHelper =>
        ref.derivedSingleDenotation(sym, TypeAlias(ref.info.extractOpaqueAlias))
      case _ =>
        ref
    }
  }
}