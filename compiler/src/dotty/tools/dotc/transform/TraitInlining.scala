package dotty.tools.dotc
package transform

import core.*
import Flags.*
import Contexts.*
import Symbols.*

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.quoted.*
import dotty.tools.dotc.inlines.InlineTraits.*
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.staging.StagingLevel
import dotty.tools.dotc.core.Constants.*

import scala.collection.mutable.ListBuffer
import javax.xml.transform.Templates

/** TODO */
class TraitInlining extends MacroTransform, DenotTransformer {
  self =>

  import tpd.*

  override def phaseName: String = TraitInlining.name

  override def description: String = TraitInlining.description

  override def allowsImplicitSearch: Boolean = true

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.needsTraitInlining then
      try super.run
      catch case _: CompilationUnit.SuspendException => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val newUnits = super.runOn(units).filterNot(_.suspended)
    newUnits

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    () // TODO

  def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      tree match
        case tree: Template if needsTraitInlining(ctx.owner.asClass) =>
          cpy.Template(tree)(body = inlinedDefs(ctx.owner.asClass) ::: tree.body)
        case _ =>
          super.transform(tree)
  }

  def transform(ref: SingleDenotation)(using Context): SingleDenotation = {
    val sym = ref.symbol
    ref match {
      case ref: SymDenotation if sym.isClass && !sym.is(Module) && sym.maybeOwner.isInlineTrait =>
        val newName =
          if sym.is(Module) then (sym.name.toString + "inline$trait$").toTypeName // TODO use NameKinds
          else (sym.name.toString + "$inline$trait").toTypeName // TODO use NameKinds
        ref.copySymDenotation(name = newName)
      case ref: SymDenotation =>
        ref.info match
          case tp @ ClassInfo(_, cls, _, decls, _) if needsTraitInlining(sym.asClass) =>
            val newDecls = decls.cloneScope
            inlinedMemberSymbols(sym.asClass).foreach(newDecls.enter)
            val newInfo = tp.derivedClassInfo(decls = newDecls)
            ref.copySymDenotation(info = newInfo).copyCaches(ref, ctx.phase.next)
          case _ =>
            ref
      case _ =>
        ref
    }
  }

}

object TraitInlining:
  val name: String = "traitInlining"
  val description: String = "generate inline trait members"
