package dotty.tools.dotc
package transform

import core._
import Decorators._
import Flags._
import Types._
import Contexts._
import Symbols._
import Constants._
import ast.Trees._
import ast.{TreeTypeMap, untpd}
import util.Spans._
import tasty.TreePickler.Hole
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import typer.Implicits.SearchFailureType

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.typer.ImportInfo.withRootImports
import dotty.tools.dotc.ast.TreeMapWithImplicits

import scala.annotation.constructorOnly

/** Inlines all calls to inline methods that are not in an inline method or a quote */
class Inlining extends MacroTransform {
  import tpd._

  override def phaseName: String = Inlining.name

  override def allowsImplicitSearch: Boolean = true

  override def run(using Context): Unit =
    // if (!ctx.settings.YinlineBlackboxWhileTyping.value) // phase not needed?
    try super.run
    catch case _: CompilationUnit.SuspendException => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val newUnits = super.runOn(units).filterNot(_.suspended)
    ctx.run.checkSuspendedUnits(newUnits)
    newUnits

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
        new TreeTraverser {
          def traverse(tree: Tree)(using Context): Unit =
            tree match
              case _: GenericApply if tree.symbol.isQuote =>
                traverseChildren(tree)(using StagingContext.quoteContext)
              case _: GenericApply if tree.symbol.isExprSplice =>
                traverseChildren(tree)(using StagingContext.spliceContext)
              case tree: RefTree if !Inliner.inInlineMethod && StagingContext.level == 0 =>
                assert(!tree.symbol.isInlineMethod, tree.show)
              case _ =>
                traverseChildren(tree)
        }.traverse(tree)
      case _ =>
    }

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new InliningTreeMap().transform(tree)
  }

  private class InliningTreeMap extends TreeMapWithImplicits {
    override def transform(tree: Tree)(using Context): Tree = {
      tree match
        case tree: DefTree =>
          if tree.symbol.is(Inline) then tree
          else super.transform(tree)
        case _: Typed | _: Block =>
          super.transform(tree)
        case _ if Inliner.isInlineable(tree) && !tree.tpe.widen.isInstanceOf[MethodOrPoly] && StagingContext.level == 0 =>
          val tree1 = super.transform(tree)
          if tree1.tpe.isError then tree1
          else
            val inlined = Inliner.inlineCall(tree1)
            if tree1 eq inlined then inlined
            else transform(inlined) // TODO can this be removed if `needsStaging` is set in `Inliner`?
        case _: GenericApply if tree.symbol.isQuote =>
          ctx.compilationUnit.needsStaging = true
          super.transform(tree)(using StagingContext.quoteContext)
        case _: GenericApply if tree.symbol.isExprSplice =>
          super.transform(tree)(using StagingContext.spliceContext)
        case _ =>
          super.transform(tree)
    }
  }
}

object Inlining {
  val name: String = "inlining"
}

////// FIXME

////// Issue: dotty.tools.dotc.transform.YCheckPositions$$anon$1.traverse(YCheckPositions.scala:33)
// tests/run/summonAll.scala
// tests/run/lst
