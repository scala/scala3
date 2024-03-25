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
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.staging.StagingLevel
import dotty.tools.dotc.core.Constants.*

import scala.collection.mutable.ListBuffer
import javax.xml.transform.Templates

/** TODO */
class TraitInlining extends MacroTransform, InfoTransformer {
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

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type = {
    tp match
      case tp @ ClassInfo(_, cls, _, decls, _) if needsTraitInlining(sym.asClass) =>
        val newDecls = decls.cloneScope
        inlinedMemberSymbols(sym.asClass).foreach(newDecls.enter)
        tp.derivedClassInfo(decls = newDecls)
      case _ =>
        tp
  }
}

object TraitInlining:
  val name: String = "traitInlining"
  val description: String = "generate inline trait members"
