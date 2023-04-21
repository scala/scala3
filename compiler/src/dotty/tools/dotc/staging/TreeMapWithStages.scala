package dotty.tools.dotc
package staging

import dotty.tools.dotc.ast.{TreeMapWithImplicits, tpd}
import dotty.tools.dotc.config.Printers.staging
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.staging.StagingLevel.*

import scala.collection.mutable

/** TreeMap that keeps track of staging levels using StagingLevel. */
abstract class TreeMapWithStages extends TreeMapWithImplicits {
  import tpd._

  /** If we are inside a quote or a splice */
  private[this] var inQuoteOrSplice = false

  /** If we are inside a quote or a splice */
  protected def isInQuoteOrSplice: Boolean = inQuoteOrSplice

  /** Transform the quote `quote` which contains the quoted `body`.
   *
   *  - `quoted.runtime.Expr.quote[T](<body0>)`  --> `quoted.runtime.Expr.quote[T](<body>)`
   *  - `quoted.Type.of[<body0>](quotes)`  --> `quoted.Type.of[<body>](quotes)`
   */
  protected def transformQuotation(body: Tree, quote: Apply)(using Context): Tree =
    if body.isTerm then
      cpy.Apply(quote)(quote.fun, body :: Nil)
    else
      val TypeApply(fun, _) = quote.fun: @unchecked
      cpy.Apply(quote)(cpy.TypeApply(quote.fun)(fun, body :: Nil), quote.args)

  /** Transform the expression splice `splice` which contains the spliced `body`. */
  protected def transformSplice(body: Tree, splice: Apply)(using Context): Tree

  /** Transform the type splice `splice` which contains the spliced `body`. */
  protected def transformSpliceType(body: Tree, splice: Select)(using Context): Tree

  override def transform(tree: Tree)(using Context): Tree =
    if (tree.source != ctx.source && tree.source.exists)
      transform(tree)(using ctx.withSource(tree.source))
    else reporting.trace(i"StagingTransformer.transform $tree at $level", staging, show = true) {
      def dropEmptyBlocks(tree: Tree): Tree = tree match {
        case Block(Nil, expr) => dropEmptyBlocks(expr)
        case _ => tree
      }

      tree match {
        case Apply(Select(QuotedTypeOf(SplicedType(t)), _), _) =>
          // Optimization: `quoted.Type.of[x.Underlying]` --> `x`
          transform(t)

        case tree @ QuotedTypeOf(quotedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try transformQuotation(quotedTree, tree)
          finally inQuoteOrSplice = old

        case tree @ QuotedExpr(quotedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(quotedTree) match {
            case SplicedExpr(t) =>
              // Optimization: `'{ $x }` --> `x`
              // and adapt the refinement of `Quotes { type reflect: ... } ?=> Expr[T]`
              transform(t).asInstance(tree.tpe)
            case _ => transformQuotation(quotedTree, tree)
          }
          finally inQuoteOrSplice = old

        case tree @ SplicedExpr(splicedTree) =>
          val old = inQuoteOrSplice
          inQuoteOrSplice = true
          try dropEmptyBlocks(splicedTree) match {
            case QuotedExpr(t) =>
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
          val defSyms = stats.collect { case defTree: DefTree => defTree.symbol }
          super.transform(tree)(using symbolsInCurrentLevel(defSyms))

        case CaseDef(pat, guard, body) =>
          super.transform(tree)(using symbolsInCurrentLevel(tpd.patVars(pat)))

        case (_:Import | _:Export) =>
          tree

        case _: Template =>
          val decls = tree.symbol.owner.info.decls.toList
          super.transform(tree)(using symbolsInCurrentLevel(decls))

        case LambdaTypeTree(tparams, body) =>
          super.transform(tree)(using symbolsInCurrentLevel(tparams.map(_.symbol)))

        case tree: DefTree =>
          val paramSyms = tree match
            case tree: DefDef => tree.paramss.flatten.map(_.symbol)
            case _ => Nil
          super.transform(tree)(using symbolsInCurrentLevel(tree.symbol :: paramSyms))

        case _ =>
          super.transform(tree)
      }
    }
}
