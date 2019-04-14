package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeMapWithImplicits, TreeTypeMap, tpd, untpd}
import dotty.tools.dotc.config.Printers.staging
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.util.Spans._
import dotty.tools.dotc.util.{Property, SourcePosition}
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.typer.Implicits.SearchFailureType
import dotty.tools.dotc.typer.Inliner

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

  /** The quotation level of the definition of the locally defined symbol */
  protected def levelOf(sym: Symbol): Option[Int] = levelOfMap.get(sym)

  /** Localy defined symbols seen so far by `StagingTransformer.transform` */
  protected def localSymbols: List[Symbol] = enteredSyms

  /** Enter staging level of symbol defined by `tree`, if applicable. */
  private def markDef(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: DefTree =>
      val sym = tree.symbol
      if ((sym.isClass || sym.maybeOwner.isTerm) && !levelOfMap.contains(sym)) {
        levelOfMap(sym) = level
        enteredSyms = sym :: enteredSyms
      }
    case _ =>
  }

  /** Transform the quote `quote` which contains the quoted `body`. */
  protected def transformQuotation(body: Tree, quote: Tree)(implicit ctx: Context): Tree = {
    quote match {
      case quote: Apply => cpy.Apply(quote)(quote.fun, body :: Nil)
      case quote: TypeApply => cpy.TypeApply(quote)(quote.fun, body :: Nil)
    }
  }

  /** Transform the splice `splice` which contains the spliced `body`. */
  protected def transformSplice(body: Tree, splice: Tree)(implicit ctx: Context): Tree

  override def transform(tree: Tree)(implicit ctx: Context): Tree = {
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(ctx.withSource(tree.source))
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

        case Quoted(quotedTree) =>
          dropEmptyBlocks(quotedTree) match {
            case Spliced(t) => transform(t) // '{ $x } --> x
            case _ => transformQuotation(quotedTree, tree)
          }

        case tree @ Spliced(splicedTree) =>
          dropEmptyBlocks(splicedTree) match {
            case Quoted(t) => transform(t) // ${ 'x } --> x
            case _ => transformSplice(splicedTree, tree)
          }

        case Block(stats, _) =>
          val last = enteredSyms
          stats.foreach(markDef)
          mapOverTree(last)

        case CaseDef(pat, guard, body) =>
          val last = enteredSyms
          // mark all bindings
          new TreeTraverser {
            def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
              case Quoted(t) => traverse(t)(quoteContext)
              case Splice(t) => traverse(t)(spliceContext)
              case _ =>
                markDef(tree)
                traverseChildren(tree)
            }
          }.traverse(pat)
          mapOverTree(last)

        case _: Import =>
          tree

        case _ =>
          markDef(tree)
          mapOverTree(enteredSyms)
      }
    }
  }
}


object TreeMapWithStages {
  import tpd._

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[mutable.HashMap[Symbol, Int]]

  /** Initial context for a StagingTransformer transformation. */
  def freshStagingContext(implicit ctx: Context): Context =
    ctx.fresh.setProperty(LevelOfKey, new mutable.HashMap[Symbol, Int])

}