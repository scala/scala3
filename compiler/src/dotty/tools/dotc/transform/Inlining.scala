package dotty.tools.dotc
package transform

import ast.tpd
import ast.Trees.*
import ast.TreeMapWithTrackedStats
import core.*
import Flags.*
import Decorators.*
import Contexts.*
import Symbols.*
import Decorators.*
import config.Printers.inlining
import DenotTransformers.IdentityDenotTransformer
import MacroAnnotations.hasMacroAnnotation
import inlines.Inlines
import quoted.*
import staging.StagingLevel
import util.Property

import scala.collection.mutable

/** Inlines all calls to inline methods that are not in an inline method or a quote */
class Inlining extends MacroTransform, IdentityDenotTransformer {
  self =>

  import tpd.*

  override def phaseName: String = Inlining.name

  override def description: String = Inlining.description

  override def allowsImplicitSearch: Boolean = true

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.needsInlining || ctx.compilationUnit.hasMacroAnnotations then
      super.run

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
        new TreeTraverser {
          def traverse(tree: Tree)(using Context): Unit =
            tree match
              case tree: RefTree if !Inlines.inInlineMethod && StagingLevel.level == 0 =>
                assert(!tree.symbol.isInlineMethod, tree.show)
              case _ =>
                traverseChildren(tree)
        }.traverse(tree)
      case _ =>
    }

  def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      InliningTreeMap().transform(tree)
  }

  private class InliningTreeMap extends TreeMapWithTrackedStats {

    /** List of top level classes added by macro annotation in a package object.
     *  These are added to the PackageDef that owns this particular package object.
     */
    private val newTopClasses = MutableSymbolMap[mutable.ListBuffer[Tree]]()

    override def transform(tree: Tree)(using Context): Tree = {
      tree match
        case tree: MemberDef =>
          // Fetch the latest tracked tree (It might have already been transformed by its companion)
          transformMemberDef(getTracked(tree.symbol).getOrElse(tree))
        case _: Typed | _: Block =>
          super.transform(tree)
        case _: PackageDef =>
          super.transform(tree) match
            case tree1: PackageDef  =>
              newTopClasses.get(tree.symbol.moduleClass) match
                case Some(topClasses) =>
                  newTopClasses.remove(tree.symbol.moduleClass)
                  val newStats = tree1.stats ::: topClasses.result()
                  cpy.PackageDef(tree1)(tree1.pid, newStats)
                case _ => tree1
            case tree1 => tree1
        case _ =>
          if tree.isType then tree
          else if Inlines.needsInlining(tree) then
            tree match
              case tree: UnApply =>
                val fun1 = Inlines.inlinedUnapplyFun(tree.fun)
                super.transform(cpy.UnApply(tree)(fun = fun1))
              case _ =>
                val tree1 = super.transform(tree)
                if tree1.tpe.isError then tree1
                else Inlines.inlineCall(tree1)
          else super.transform(tree)
    }

    private def transformMemberDef(tree: MemberDef)(using Context) : Tree =
      if tree.symbol.is(Inline) then tree
      else if tree.symbol.is(Param) then
        super.transform(tree)
      else if
        !tree.symbol.isPrimaryConstructor
        && StagingLevel.level == 0
        && tree.symbol.hasMacroAnnotation
      then
        // Fetch the companion's tree
        val companionSym =
          if tree.symbol.is(ModuleClass) then tree.symbol.companionClass
          else if tree.symbol.is(ModuleVal) then NoSymbol
          else tree.symbol.companionModule.moduleClass

        // Expand and process MacroAnnotations
        val companion = getTracked(companionSym)
        val (trees, newCompanion) = MacroAnnotations.expandAnnotations(tree, companion)

        // Enter the new symbols & Update the tracked trees
        (newCompanion.toList ::: trees).foreach: tree =>
          MacroAnnotations.enterMissingSymbols(tree, self)

        // Perform inlining on the expansion of the annotations
        val trees1 = trees.map(super.transform)
        trees1.foreach(updateTracked)
        if newCompanion ne companion then
          newCompanion.map(super.transform).foreach(updateTracked)

        // Find classes added to the top level from a package object
        val (topClasses, trees2) =
          if ctx.owner.isPackageObject then trees1.partition(_.symbol.owner == ctx.owner.owner)
          else (Nil, trees1)
        if topClasses.nonEmpty then
          newTopClasses.getOrElseUpdate(ctx.owner.owner, new mutable.ListBuffer) ++= topClasses
        flatTree(trees2)
      else
        updateTracked(super.transform(tree))
    end transformMemberDef

  }

}

object Inlining:
  val name: String = "inlining"
  val description: String = "inline and execute macros"
