package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{TreeMapWithImplicits, tpd}
import dotty.tools.dotc.config.Printers.staging
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.util.Property

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

  /** Locally defined symbols seen so far by `StagingTransformer.transform` */
  protected def localSymbols: List[Symbol] = enteredSyms

  /** If we are inside a quote or a splice */
  protected def isInQuoteOrSplice: Boolean = inQuoteOrSplice

  /** Enter staging level of symbol defined by `tree` */
  private def markSymbol(sym: Symbol)(using Context): Unit =
    if level != 0 && !levelOfMap.contains(sym) then
      levelOfMap(sym) = level
      enteredSyms = sym :: enteredSyms

  /** Enter staging level of symbol defined by `tree`, if applicable. */
  private def markDef(tree: Tree)(using Context): Unit = tree match {
    case tree: DefTree => markSymbol(tree.symbol)
    case _ =>
  }

  /** Transform the quote `quote` which contains the quoted `body`.
   *
   *  - `quoted.runtime.Expr.quote[T](<body0>)`  --> `quoted.runtime.Expr.quote[T](<body>)`
   *  - `quoted.Type.of[<body0>](quotes)`  --> `quoted.Type.of[<body>](quotes)`
   */
  protected def transformQuotation(body: Tree, quote: Apply)(using Context): Tree =
    if body.isTerm then
      cpy.Apply(quote)(quote.fun, body :: Nil)
    else
      val TypeApply(fun, _) = quote.fun
      cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, body :: Nil), quote.args)

  /** Transform the expression splice `splice` which contains the spliced `body`. */
  protected def transformSplice(body: Tree, splice: Apply)(using Context): Tree

  /** Transform the type splice `splice` which contains the spliced `body`. */
  protected def transformSpliceType(body: Tree, splice: Select)(using Context): Tree

  override def transform(tree: Tree)(using Context): Tree =
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
            case SplicedType(t) =>
              // Optimization: `quoted.Type.of[x.Underlying]` --> `x`
              transform(t)
            case _ =>
              super.transform(tree)

        case tree @ Quoted(quotedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(quotedTree) match {
            case Spliced(t) =>
              // Optimization: `'{ $x }` --> `x`
              // and adapt the refinement of `Quotes { type reflect: ... } ?=> Expr[T]`
              transform(t).asInstance(tree.tpe)
            case _ => transformQuotation(quotedTree, tree)
          }
          finally inQuoteOrSplice = old

        case tree @ Spliced(splicedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(splicedTree) match {
            case Quoted(t) =>
              // Optimization: `${ 'x }` --> `x`
              transform(t)
            case _ => transformSplice(splicedTree, tree)
          }
          finally inQuoteOrSplice = old

        case tree @ SplicedType(splicedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try transformSpliceType(splicedTree, tree)
          finally inQuoteOrSplice = old

        case Block(stats, _) =>
          val last = enteredSyms
          stats.foreach(markDef)
          mapOverTree(last)

        case CaseDef(pat, guard, body) =>
          val last = enteredSyms
          tpd.patVars(pat).foreach(markSymbol)
          mapOverTree(last)

        case (_:Import | _:Export) =>
          tree

        case _ =>
          markDef(tree)
          mapOverTree(enteredSyms)
      }
    }
}


object TreeMapWithStages {

  /** A key to be used in a context property that caches the `levelOf` mapping */
  private val LevelOfKey = new Property.Key[mutable.HashMap[Symbol, Int]]

  /** Initial context for a StagingTransformer transformation. */
  def freshStagingContext(using Context): Context =
    ctx.fresh.setProperty(LevelOfKey, new mutable.HashMap[Symbol, Int])

}
