package dotty.tools.dotc
package transform

import core._
import Flags._
import Contexts._
import Symbols._
import SymUtils._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.ast.TreeMapWithImplicits

import scala.annotation.tailrec
import scala.collection.mutable


/** Inlines all calls to inline methods that are not in an inline method or a quote */
class Inlining extends MacroTransform {
  import tpd._

  override def phaseName: String = Inlining.name

  override def description: String = Inlining.description

  override def allowsImplicitSearch: Boolean = true

  override def run(using ctx0: Context): Unit =
    if ctx0.compilationUnit.needsInlining || ctx0.compilationUnit.hasMacroAnnotations then
      try
        val ctx = QuotesCache.init(ctx0.fresh)
        super.run(using ctx)
      catch case _: CompilationUnit.SuspendException => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val newUnits = super.runOn(units).filterNot(_.suspended)
    ctx.run.nn.checkSuspendedUnits(newUnits)
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
              case tree: RefTree if !Inlines.inInlineMethod && StagingContext.level == 0 =>
                assert(!tree.symbol.isInlineMethod, tree.show)
              case _ =>
                traverseChildren(tree)
        }.traverse(tree)
      case _ =>
    }

  def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new InliningTreeMap().transform(tree)
  }

  private class InliningTreeMap extends TreeMapWithImplicits {
    override def transform(tree: Tree)(using Context): Tree = {
      tree match
        case tree: DefTree =>
          if tree.symbol.is(Inline) then tree
          else
            tree match
              case _: Bind => super.transform(tree)
              case tree if tree.symbol.is(Param) => super.transform(tree)
              case tree if !tree.symbol.isPrimaryConstructor =>
                val trees = MacroAnnotationTransformer.transform(List(tree), Set(tree.symbol))
                flatTree(trees.map(super.transform(_)))
              case tree => super.transform(tree)
        case ObjectTrees(valT, clsT) =>
          val trees = MacroAnnotationTransformer.transform(List(Thicket(valT, clsT)), Set(valT.symbol, clsT.symbol))
          assert(trees.size >= 2)
          flatTree(trees.map{ tree =>
            if tree.symbol.is(Inline) then tree
            else super.transform(tree)
          })
        case _: Typed | _: Block =>
          super.transform(tree)
        case _ if Inlines.needsInlining(tree) =>
          val tree1 = super.transform(tree)
          if tree1.tpe.isError then tree1
          else Inlines.inlineCall(tree1)
        case _: GenericApply if tree.symbol.isQuote =>
          super.transform(tree)(using StagingContext.quoteContext)
        case _: GenericApply if tree.symbol.isExprSplice =>
          super.transform(tree)(using StagingContext.spliceContext)
        case _ =>
          super.transform(tree)
    }

    override def transformStats[T](trees: List[Tree], exprOwner: Symbol, wrapResult: List[Tree] => Context ?=> T)(using Context): T  =
      @tailrec
      def loop(mapped: mutable.ListBuffer[Tree] | Null, unchanged: List[Tree], pending: List[Tree])(using Context): T =
        inline def recur(unchange: Boolean, stat1: Tree, rest: List[Tree])(using Context): T =
          if unchange then
            loop(mapped, unchanged, rest)
          else
            val buf = if mapped == null then new mutable.ListBuffer[Tree] else mapped
            var xc = unchanged
            while xc ne pending do
              buf += xc.head
              xc = xc.tail
            stat1 match
              case Thicket(stats1) => buf ++= stats1
              case _ => buf += stat1
            loop(buf, rest, rest)

        pending match
          case valT :: clsT :: rest if valT.symbol.is(ModuleVal) && clsT.symbol.is(ModuleClass) &&
                                       valT.symbol.moduleClass == clsT.symbol =>
            val stat1 = transform(Thicket(List(valT, clsT)))(using ctx)
            val unchange = stat1 match
              case Thicket(List(valT1, clsT1)) => (valT eq valT1) && (clsT eq clsT1)
              case _ => false
            recur(unchange, stat1, rest)(using ctx)
          case stat :: rest =>
            val statCtx = stat match
              case _: DefTree | _: ImportOrExport => ctx
              case _ => ctx.exprContext(stat, exprOwner)
            val stat1 = transform(stat)(using statCtx)
            val restCtx = stat match
              case stat: Import => ctx.importContext(stat, stat.symbol)
              case _ => ctx
            recur(stat1 eq stat, stat1, rest)(using restCtx)
          case nil =>
            wrapResult(
              if mapped == null then unchanged
              else mapped.prependToList(unchanged))
      loop(null, trees, trees)
  }
}

object Inlining:
  val name: String = "inlining"
  val description: String = "inline and execute macros"
