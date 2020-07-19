package dottyBench.tools.dotc
package transform

import dottyBench.tools.dotc.ast.Trees._
import dottyBench.tools.dotc.ast.{TreeMapWithImplicits, TreeTypeMap, tpd, untpd}
import dottyBench.tools.dotc.config.Printers.staging
import dottyBench.tools.dotc.core.Constants._
import dottyBench.tools.dotc.core.Decorators._
import dottyBench.tools.dotc.core.Flags._
import dottyBench.tools.dotc.core.quoted._
import dottyBench.tools.dotc.core.NameKinds._
import dottyBench.tools.dotc.core.Types._
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.StagingContext._
import dottyBench.tools.dotc.core.StdNames._
import dottyBench.tools.dotc.core.Symbols._
import dottyBench.tools.dotc.core.tasty.TreePickler.Hole
import dottyBench.tools.dotc.util.Spans._
import dottyBench.tools.dotc.util.{Property, SourcePosition}
import dottyBench.tools.dotc.transform.SymUtils._
import dottyBench.tools.dotc.typer.Implicits.SearchFailureType

import scala.collection.mutable
import scala.annotation.constructorOnly

/** The main transformer class
 *  @param  level      the current level, where quotes add one and splices subtract one level.
 *                     The initial level is 0, a level `l` where `l > 0` implies code has been quoted `l` times
 *                     and `l == -1` is code inside a top level splice (in an inline method).
 *  @param  levels     a stacked map from symbols to the levels in which they were defined
 */
abstract class TreeMapWithStages(@constructorOnly ictx: Context) extends TreeMapWithImplicits {

  import tpd._
  import TreeMapWithStages._

  /** A map from locally defined symbols to their definition quotation level */
  private[this] val levelOfMap: mutable.HashMap[Symbol, Int] = ictx.property(LevelOfKey).get

  /** A stack of entered symbols, to be unwound after scope exit */
  private[this] var enteredSyms: List[Symbol] = Nil

  /** If we are inside a quote or a splice */
  private[this] var inQuoteOrSplice = false

  /** The quotation level of the definition of the locally defined symbol */
  protected def levelOf(sym: Symbol): Int = levelOfMap.getOrElse(sym, 0)

  /** Localy defined symbols seen so far by `StagingTransformer.transform` */
  protected def localSymbols: List[Symbol] = enteredSyms

  /** If we are inside a quote or a splice */
  protected def isInQuoteOrSplice: Boolean = inQuoteOrSplice

  /** Enter staging level of symbol defined by `tree` */
  private def markSymbol(sym: Symbol)(using Ctx, CState): Unit =
    if level != 0 && !levelOfMap.contains(sym) then
      levelOfMap(sym) = level
      enteredSyms = sym :: enteredSyms

  /** Enter staging level of symbol defined by `tree`, if applicable. */
  private def markDef(tree: Tree)(using Ctx, CState): Unit = tree match {
    case tree: DefTree => markSymbol(tree.symbol)
    case _ =>
  }

  /** Transform the quote `quote` which contains the quoted `body`. */
  protected def transformQuotation(body: Tree, quote: Tree)(using Ctx, CState): Tree =
    quote match {
      case quote: Apply => cpy.Apply(quote)(quote.fun, body :: Nil)
      case quote: TypeApply => cpy.TypeApply(quote)(quote.fun, body :: Nil)
    }

  /** Transform the splice `splice` which contains the spliced `body`. */
  protected def transformSplice(body: Tree, splice: Tree)(using Ctx, CState): Tree

  override def transform(tree: Tree)(using Ctx, CState): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else reporting.trace(i"StagingTransformer.transform $tree at $level", staging, show = true) {
      def mapOverTree(lastEntered: List[Symbol]) =
        try super.transform(tree)
        finally
          while (enteredSyms ne lastEntered) {
            levelOfMap -= enteredSyms.head
            enteredSyms = enteredSyms.tail
          }

      def dropEmptyBlocks(tree: Tree): Tree = tree match {
        case Block(Nil, expr) => dropEmptyBlocks(expr)
        case _ => tree
      }

      tree match {
        case Apply(Select(Quoted(quotedTree), _), _) if quotedTree.isType =>
          dropEmptyBlocks(quotedTree) match
            case Spliced(t) =>
              // '[ x.$splice ] --> x
              transform(t)
            case _ =>
              super.transform(tree)

        case Quoted(quotedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(quotedTree) match {
            case Spliced(t) =>
              // '{ $x } --> x
              // and adapt the refinment of `QuoteContext { type tasty: ... } ?=> Expr[T]`
              transform(t).asInstance(tree.tpe)
            case _ => transformQuotation(quotedTree, tree)
          }
          finally inQuoteOrSplice = old

        case tree @ Spliced(splicedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(splicedTree) match {
            case Quoted(t) => transform(t) // ${ 'x } --> x
            case _ => transformSplice(splicedTree, tree)
          }
          finally inQuoteOrSplice = old

        case Block(stats, _) =>
          val last = enteredSyms
          stats.foreach(markDef)
          mapOverTree(last)

        case CaseDef(pat, guard, body) =>
          val last = enteredSyms
          tpd.patVars(pat).foreach(markSymbol)
          mapOverTree(last)

        case _: Import =>
          tree

        case _ =>
          markDef(tree)
          mapOverTree(enteredSyms)
      }
    }
}


object TreeMapWithStages {
  import tpd._

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[mutable.HashMap[Symbol, Int]]

  /** Initial context for a StagingTransformer transformation. */
  def freshStagingContext(using Ctx, CState): Context =
    combinedContext.fresh.setProperty(LevelOfKey, new mutable.HashMap[Symbol, Int])
}