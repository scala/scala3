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

import scala.annotation.constructorOnly

/** Inlines all calls to inline methods that are not in an inline method or a quote */
class Inlining extends MacroTransform {
  import tpd._

  override def phaseName: String = Inlining.name

  override def allowsImplicitSearch: Boolean = true

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case tree: RefTree if !Inliner.inInlineMethod && StagingContext.level == 0 =>
        assert(!tree.symbol.isInlineMethod)
      case _ =>
    }

  protected def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      tree match
        case tree: DefTree =>
          if tree.symbol.is(Inline) then tree
          else super.transform(tree)
        case _: Typed | _: Block =>
          super.transform(tree)
        case _ if Inliner.isInlineable(tree) && !tree.tpe.widen.isInstanceOf[MethodOrPoly] && StagingContext.level == 0 =>
          val tree1 = super.transform(tree)
          val inlined = Inliner.inlineCall(tree1)
          if tree1 eq inlined then inlined
          else transform(inlined)
        case _: TypeApply if tree.symbol.isQuote =>
          ctx.compilationUnit.needsStaging = true
          super.transform(tree)
        case _ =>
          super.transform(tree)

  }
}

object Inlining {
  val name: String = "inlining"
}
