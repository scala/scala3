package dotty.tools.dotc
package transform

import core._
import Flags._
import Contexts._
import Symbols._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._

import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.config.Feature.inlineTraitsEnabledSomewhere

class PostSpecializeInlineTraits extends Phase:
  import tpd.*
  override def phaseName: String = PostSpecializeInlineTraits.name
  
  override def description: String = PostSpecializeInlineTraits.description
  
  override def isEnabled(using Context): Boolean = inlineTraitsEnabledSomewhere

  override protected def run(using Context): Unit = 
    val unit = ctx.compilationUnit
    if unit.hasSpecializations then
      val inlinedMembers = ctx.inlineTraitState.inlineOrigins.flatMap {
        case (inlined, origins) =>
          origins.collect {
            case original if !inlined.isType && inlined.name == original.name =>
              (inlined.owner, original) -> inlined
          }
      }.toMap
      
      def adaptInlinedMembers(tree: Tree): Tree =
        new TreeMap {
          override def transform(tree: Tree)(using Context): Tree = tree match
            case select: Select if !select.qualifier.isInstanceOf[Super] && !select.symbol.isType =>
              val qualifier = transform(select.qualifier)
              qualifier.tpe.widen.baseClasses
                .flatMap(owner => inlinedMembers.get((owner, select.symbol)))
                .headOption match
                  case Some(sym) =>
                    qualifier.selectWithSig(sym).withSpan(select.span).withAttachmentsFrom(select)
                  case None =>
                    cpy.Select(select)(qualifier, select.name)
            case _ => super.transform(tree)
        }.transform(tree)
        
      if inlinedMembers.nonEmpty then
        unit.tpdTree = adaptInlinedMembers(unit.tpdTree)

object PostSpecializeInlineTraits:
  val name: String = "postSpecializeInlineTraits"
  val description: String = "Fixup code after inline trait specialization"
