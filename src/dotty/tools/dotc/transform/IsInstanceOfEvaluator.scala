package dotty.tools.dotc
package transform

import dotty.tools.dotc.util.Positions._
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import core._
import Contexts.Context, Types._, Constants._, Decorators._, Symbols._
import TypeUtils._, TypeErasure._


/** Implements partial `isInstanceOf` evaluation according to the matrix on:
 *  https://github.com/lampepfl/dotty/issues/1255
 *
 *  This is a generalized solution to raising an error on unreachable match
 *  cases and warnings on other statically known results of `isInstanceOf`.
 *
 *  Steps taken:
 *
 *  1.  evalTypeApply will establish the matrix and choose the appropriate
 *      handling for the case:
 *  2.  a) handleStaticallyKnown
 *      b) falseIfUnrelated with `scrutinee <:< selector`
 *      c) handleFalseUnrelated
 *      d) leave as is (aka `happens`)
 *  3.  Rewrite according to step taken in `2`
 */
class IsInstanceOfEvaluator extends MiniPhaseTransform { thisTransformer =>

  import dotty.tools.dotc.ast.tpd._

  def phaseName = "isInstanceOfEvaluator"
  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val defn = ctx.definitions

    /** Handles the four cases of statically known `isInstanceOf`s and gives
     *  the correct warnings, or an error if statically known to be false in
     *  match
     */
    def handleStaticallyKnown(tree: Select, scrutinee: Type, selector: Type, inMatch: Boolean, pos: Position): Tree =
      if (!(scrutinee <:< selector) && inMatch) {
        ctx.error(
          s"this case is unreachable due to `${selector.show}` not being a subclass of `${scrutinee.show}`",
          Position(pos.start - 5, pos.end - 5)
        )
        rewrite(tree, to = false)
      } else if (!(scrutinee <:< selector) && !inMatch) {
        ctx.warning(
          s"this will always yield false since `${scrutinee.show}` is not a subclass of `${selector.show}` (will be optimized away)",
          pos
        )
        rewrite(tree, to = false)
      } else if (scrutinee <:< selector && !inMatch) {
        ctx.warning(
          s"this will always yield true since `${scrutinee.show}` is a subclass of `${selector.show}` (will be optimized away)",
          pos
        )
        rewrite(tree, to = true)
      } else /* if (scrutinee <:< selector && inMatch) */ rewrite(tree, to = true)

    /** Rewrites cases with unrelated types */
    def handleFalseUnrelated(tree: Select, scrutinee: Type, selector: Type, inMatch: Boolean) =
      if (inMatch) {
        ctx.error(
          s"will never match since `${selector.show}` is not a subclass of `${scrutinee.show}`",
          Position(tree.pos.start - 5, tree.pos.end - 5)
        )
        rewrite(tree, to = false)
      } else {
        ctx.warning(
          s"will always yield false since `${scrutinee.show}` is not a subclass of `${selector.show}`",
          tree.pos
        )
        rewrite(tree, to = false)
      }

    /** Rewrites the select to a boolean if `to` is false or if the qualifier
     *  is a primitive.
     *
     *  If `to` is set to true and the qualifier is not a primitive, the
     *  instanceOf is replaced by a null check, since:
     *
     *  `srutinee.isInstanceOf[Selector]` if `scrutinee eq null`
     */
    def rewrite(tree: Select, to: Boolean): Tree =
      if (!to || !tree.qualifier.tpe.widen.derivesFrom(defn.AnyRefAlias))
        Literal(Constant(to))
      else
        Apply(tree.qualifier.select(defn.Object_ne), List(Literal(Constant(null))))

    /** Attempts to rewrite TypeApply to either `scrutinee ne null` or a
     *  constant
     */
    def evalTypeApply(tree: TypeApply): Tree =
      if (tree.symbol != defn.Any_isInstanceOf) tree
      else tree.fun match {
        case s: Select => {
          val scrutinee = erasure(s.qualifier.tpe.widen)
          val selector  = erasure(tree.args.head.tpe.widen)

          val scTrait = scrutinee.typeSymbol is Flags.Trait
          val scClass =
            scrutinee.typeSymbol.isClass &&
            !(scrutinee.typeSymbol is Flags.Trait) &&
            !(scrutinee.typeSymbol is Flags.Module)

          val scClassNonFinal = scClass && !scrutinee.typeSymbol.is(Flags.Final)
          val scFinalClass    = scClass && (scrutinee.typeSymbol is Flags.Final)

          val selTrait = selector.typeSymbol is Flags.Trait
          val selClass =
            selector.typeSymbol.isClass &&
            !(selector.typeSymbol is Flags.Trait) &&
            !(selector.typeSymbol is Flags.Module)

          val selClassNonFinal = scClass && !(selector.typeSymbol is Flags.Final)
          val selFinalClass    = scClass && (selector.typeSymbol is Flags.Final)

          // Cases ---------------------------------
          val knownStatically = scFinalClass

          val falseIfUnrelated =
            (scClassNonFinal && selClassNonFinal) ||
            (scClassNonFinal && selFinalClass)    ||
            (scTrait && selFinalClass)

          val happens =
            (scClassNonFinal && selClassNonFinal) ||
            (scTrait && selClassNonFinal)         ||
            (scTrait && selTrait)

          val inMatch = s.qualifier.symbol is Flags.Case

          if (knownStatically)
            handleStaticallyKnown(s, scrutinee, selector, inMatch, tree.pos)
          else if (falseIfUnrelated && scrutinee <:< selector)
            rewrite(s, to = true)
          else if (falseIfUnrelated && !(selector <:< scrutinee))
            handleFalseUnrelated(s, scrutinee, selector, inMatch)
          else if (happens) tree
          else tree
        }

        case _ => tree
      }

    evalTypeApply(tree)
  }
}
