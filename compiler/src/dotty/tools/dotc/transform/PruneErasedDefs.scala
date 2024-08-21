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
import ast.tpd
import reporting.InlinedAnonClassWarning

import config.Feature
import Decorators.*
import dotty.tools.dotc.core.Types.MethodType

/** This phase makes all erased term members of classes private so that they cannot
 *  conflict with non-erased members. This is needed so that subsequent phases like
 *  ResolveSuper that inspect class members work correctly.
 *  The phase also replaces all expressions that appear in an erased context by
 *  default values. This is necessary so that subsequent checking phases such
 *  as IsInstanceOfChecker don't give false negatives.
 */
class PruneErasedDefs extends MiniPhase with SymTransformer { thisTransform =>
  import tpd.*
  import PruneErasedDefs.*

  override def phaseName: String = PruneErasedDefs.name

  override def description: String = PruneErasedDefs.description

  override def changesMembers: Boolean = true   // makes erased members private

  override def runsAfterGroupsOf: Set[String] = Set(RefChecks.name, ExplicitOuter.name)

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if !sym.isEffectivelyErased || !sym.isTerm || sym.is(Private) || !sym.owner.isClass then sym
    else sym.copySymDenotation(initFlags = sym.flags | Private)

  override def transformApply(tree: Apply)(using Context): Tree =
    tree.fun.tpe.widen match
      case mt: MethodType if mt.hasErasedParams =>
        cpy.Apply(tree)(tree.fun, tree.args.zip(mt.erasedParams).map((a, e) => if e then trivialErasedTree(a) else a))
      case _ =>
        tree

  override def transformValDef(tree: ValDef)(using Context): Tree =
    checkErasedInExperimental(tree.symbol)
    if !tree.symbol.isEffectivelyErased || tree.rhs.isEmpty then tree
    else cpy.ValDef(tree)(rhs = trivialErasedTree(tree.rhs))

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    def checkNoInlineAnnoClasses(tree: DefDef)(using Context): Unit =
      if tree.symbol.is(Inline) then
        new TreeTraverser {
          def traverse(tree: Tree)(using Context): Unit =
            tree match
              case tree: TypeDef if tree.symbol.isAnonymousClass =>
                report.warning(new InlinedAnonClassWarning(), tree.symbol.sourcePos)
              case _ => traverseChildren(tree)
        }.traverse(tree)

    checkNoInlineAnnoClasses(tree)
    checkErasedInExperimental(tree.symbol)
    if !tree.symbol.isEffectivelyErased || tree.rhs.isEmpty then tree
    else cpy.DefDef(tree)(rhs = trivialErasedTree(tree.rhs))

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    checkErasedInExperimental(tree.symbol)
    tree

  def checkErasedInExperimental(sym: Symbol)(using Context): Unit =
    // Make an exception for Scala 2 experimental macros to allow dual Scala 2/3 macros under non experimental mode
    if sym.is(Erased, butNot = Macro) && sym != defn.Compiletime_erasedValue && !sym.isInExperimentalScope then
      Feature.checkExperimentalFeature("erased", sym.sourcePos)
}

object PruneErasedDefs {
  import tpd.*

  val name: String = "pruneErasedDefs"
  val description: String = "drop erased definitions and simplify erased expressions"

  def trivialErasedTree(tree: Tree)(using Context): Tree =
    ref(defn.Compiletime_erasedValue).appliedToType(tree.tpe).withSpan(tree.span)
}
