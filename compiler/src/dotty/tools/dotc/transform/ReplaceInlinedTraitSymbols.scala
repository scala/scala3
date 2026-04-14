package dotty.tools.dotc
package transform

import ast.*, core._
import Flags._
import Contexts._
import Symbols._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.staging.StagingLevel
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.StdNames.{str, nme}
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.{Name, TermName}

import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

class ReplaceInlinedTraitSymbols extends MiniPhase:
  import tpd._

  override def phaseName: String = ReplaceInlinedTraitSymbols.name
  override def description: String = ReplaceInlinedTraitSymbols.description
  override def changesMembers: Boolean = true
  override def changesParents: Boolean = true
  override def runsAfter: Set[String] =  Set("desugarSpecializedTraits", "specializeInlineTraits")

  override def transformSelect(tree: Select)(using Context): Tree =
    val qualType = tree.qualifier.tpe.widenDealias
    if ctx.inlineTraitState.inlinedSymbolIsRegistered(tree.symbol, qualType) then
      val newSym = ctx.inlineTraitState.lookupInlinedSymbol(tree.symbol, qualType)
      assert(tree.symbol.isTerm)
      tree.withType(newSym.termRef)
    else
      tree
      
  override def runsAfterGroupsOf: Set[String] = Set("specializeInlineTraits")
object ReplaceInlinedTraitSymbols:
  val name: String = "replaceInlinedTraitSymbols"
  val description: String = "Replace symbols referring to inline trait members with resulting inlined member symbols. Also replace bridge method calls with specialized method calls for specialized traits."
  /* We need to replace symbols referring to inlined methods / members because otherwise we will still point
     to the parent symbol (this was resolved before we generated the new symbols) and so we won't get the efficiency gain.
     See tests/pos/inline-trait-return-ref.scala. We also need to do this outside the inline traits themselves (i.e. in
     the whole program - see tests/pos/inline-trait-parent-ref.scala) */
