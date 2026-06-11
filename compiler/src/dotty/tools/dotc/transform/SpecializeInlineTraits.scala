package dotty.tools.dotc
package transform


import core._
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

import scala.collection.mutable
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.inlines.Inlines.InlineTraitState
import dotty.tools.dotc.ast.TreeTypeMap

class SpecializeInlineTraits extends MiniPhase {

  import tpd._

  override def phaseName: String = SpecializeInlineTraits.name

  override def description: String = SpecializeInlineTraits.description

  override def changesMembers: Boolean = true

  override def changesParents: Boolean = true

  private def inlineTraitCtx(using Context): Context = ctx.fresh.setInlineTraitState(ctx.inlineTraitState.copyInPhase(InlineTraitState.InlineContext.InlineTraits))    

  private val seen = mutable.HashSet[Symbol]() // Ensure we don't inline into the same child class multiple times even though we repeatedly check for nested inlines.

  private def inlineInlineTraitsIfNew(tree: Tree)(using Context) = 
    if !seen.contains(tree.symbol) then
      seen.add(tree.symbol)
      Inlines.inlineParentInlineTraits(tree)(using inlineTraitCtx)
    else
      tree

  private def shouldInline(tree: Tree)(using Context) = Inlines.needsInlining(tree)(using inlineTraitCtx)

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    new TreeMapWithPreciseStatContexts { // We need to inline recursively because inlining may create further opportunities for inlining. 
                                         // Notably this does limit the composition potential of this miniphase.
                                         // TODO: We might be able to fix that with transformFollowing 
      override def transform(tree: Tree)(using Context): Tree = 
        tree match {
        case tree: TypeDef if tree.symbol.isInlineTrait =>
          val tree1 = Inlines.checkAndTransformInlineTrait(tree)
          val tree2 = if shouldInline(tree1) then inlineInlineTraitsIfNew(tree1) else tree1
          super.transform(tree2) // We may need to inline inline traits into the bodies of methods defined inside inline traits.
        case tree: TypeDef if shouldInline(tree) =>
          if tree.symbol.isAllOf(Trait, butNot = Inline) then
            val problemParents = tree.symbol.info.parents.filter(
              p => p.classSymbol.isInlineTrait 
                  && p.classSymbol.primaryConstructor.paramSymss.exists(paramList => paramList.nonEmpty && paramList.head.isTerm)
            )
            problemParents.foreach( p =>
              val message = if p.typeSymbol.isSpecializedTrait then "Specialized traits may not be extended by ordinary traits. They may only be extended by classes, objects or inline/specialized traits."
                                                                else s"Only parameterless inline traits may be extended by ordinary traits. Make ${tree.symbol} inline or remove inline ${p.typeSymbol}'s parameter list."
                
              report.error(message, tree.srcPos)
            )
          val tree1 =
            if tree.symbol.isInlineTrait then 
              inlineInlineTraitsIfNew(Inlines.checkAndTransformInlineTrait(tree))
            else inlineInlineTraitsIfNew(tree)
          super.transform(tree1)
        case t => super.transform(t)  
      }
    }.transform(tree)

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      // TODO: check that things are inlined properly
      case _ =>
    }

  private object ConcreteParentStripper extends TreeAccumulator[Tree] {
    def apply(tree: Tree)(using Context): Tree = apply(tree, tree)

    override def apply(x: Tree, tree: Tree)(using Context): Tree = tree match {
      case ident: Ident => ident
      case tpt: TypeTree => tpt
      case _ => foldOver(x, tree)
    }
  }
}

object SpecializeInlineTraits:
  val name: String = "specializeInlineTraits"
  val description: String = "inline the code of inline traits"

  private[transform] def newInnerClassName(name: Name): name.ThisName = name ++ str.INLINE_TRAIT_INNER_CLASS_SUFFIX
