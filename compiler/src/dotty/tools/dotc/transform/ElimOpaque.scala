package dotty.tools
package dotc
package transform

import core.*
import dotty.tools.dotc.transform.MegaPhase.*
import Flags.*
import Types.*
import Contexts.*
import Symbols.*
import Denotations.{SingleDenotation, NonSymSingleDenotation}
import SymDenotations.SymDenotation
import DenotTransformers.*
import Names.*

object ElimOpaque {
  val name: String = "elimOpaque"
  val description: String = "turn opaque into normal aliases"
}

/** Rewrites opaque type aliases to normal alias types */
class ElimOpaque extends MiniPhase with DenotTransformer {
  thisPhase =>
  import ast.tpd.*

  override def phaseName: String = ElimOpaque.name

  override def description: String = ElimOpaque.description

  // Override checks need to take place before treating opaque types as aliases
  override def runsAfterGroupsOf: Set[String] = Set(typer.RefChecks.name)

  // base types of opaque aliases change
  override def changesBaseTypes = true

  def transform(ref: SingleDenotation)(using Context): SingleDenotation = {
    val sym = ref.symbol
    ref match {
      case ref: SymDenotation if sym.isOpaqueAlias =>
        ref.copySymDenotation(
          info = TypeAlias(ref.opaqueAlias),
          initFlags = ref.flags &~ (Opaque | Deferred))
      case ref: SymDenotation if sym.containsOpaques =>
        def stripOpaqueRefinements(tp: Type): Type = tp match {
          case RefinedType(parent, rname, TypeAlias(_))
          if ref.info.decl(rname).symbol.isOpaqueAlias => stripOpaqueRefinements(parent)
          case _ => tp
        }
        val cinfo = sym.asClass.classInfo
        val strippedSelfType = stripOpaqueRefinements(cinfo.selfType)
        ref.copySymDenotation(
          info = cinfo.derivedClassInfo(selfInfo = strippedSelfType),
          initFlags = ref.flags &~ Opaque)
      case ref: NonSymSingleDenotation if sym.isOpaqueAlias =>
        ref.derivedSingleDenotation(sym, TypeAlias(sym.opaqueAlias.asSeenFrom(ref.prefix, sym.owner)))
      case _ =>
        ref
    }
  }

  override def transformIsNoOpFor(ref: SingleDenotation)(using Context): Boolean =
    // Both SymDenotation arms of `transform` are keyed on `isOpaqueAlias` or
    // `containsOpaques`, i.e. on the Opaque flag; for a SymDenotation, `sym.denot`
    // at this phase is `ref` itself, so `!ref.is(Opaque)` read off `ref`'s own flags
    // makes both arms dead and `transform` falls through to `case _ => ref`. The
    // NonSymSingleDenotation arm stays ungated: it consults the SYMBOL's opaqueness,
    // and the symbol's last known denotation could already be the post-elimOpaque,
    // Opaque-stripped one.
    ref match
      case ref: SymDenotation => !ref.is(Opaque)
      case _ => false

  /** Resolve overloading of `==` and `!=` methods with the representation
   *  types in order to avoid boxing.
   */
  override def transformApply(tree: Apply)(using Context): Tree =
    val sym = tree.symbol
    if sym == defn.Any_== || sym == defn.Any_!= then
      tree match
        case Apply(Select(receiver, name: TermName), args)
        if atPhase(thisPhase)(receiver.tpe.widenDealias.typeSymbol.isOpaqueAlias) =>
          applyOverloaded(receiver, name, args, Nil, defn.BooleanType)
        case _ =>
          tree
    else
      tree
}
