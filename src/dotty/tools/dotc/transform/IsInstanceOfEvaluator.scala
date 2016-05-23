package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.util.Positions._
import dotty.tools.dotc.core.TypeErasure._
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class IsInstanceOfEvaluator extends MiniPhaseTransform { thisTransformer =>

  import dotty.tools.dotc.ast.tpd._

  def phaseName = "reachabilityChecker"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val defn = ctx.definitions

    def handleStaticallyKnown(tree: TypeApply, scrutinee: Type, selector: Type, inMatch: Boolean, pos: Position): Tree =
      if (!(scrutinee <:< selector) && inMatch) {
        ctx.error(s"this case is unreachable due to `${selector.show}` not being a subclass of `${scrutinee.show}`", pos)
        tree
      } else if (!(scrutinee <:< selector) && !inMatch) {
        ctx.warning(s"this will always yield false since `${scrutinee.show}` is not a subclass of `${selector.show}` (will be optimized away)", pos)
        rewrite(tree, to = false)
      } else if (scrutinee <:< selector && !inMatch) {
        ctx.warning(s"this will always yield true since `${scrutinee.show}` is a subclass of `${selector.show}` (will be optimized away)", pos)
        rewrite(tree, to = true)
      } else /* if (scrutinee <:< selector && inMatch) */ rewrite(tree, to = true)

    def rewrite(tree: TypeApply, to: Boolean): Tree = tree

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

          // Doesn't need to be calculated (since true if others are false)
          //val happens =
          //  (scClassNonFinal && selClassNonFinal) ||
          //  (scTrait && selClassNonFinal)         ||
          //  (scTrait && selTrait)

          val inMatch = s.qualifier.symbol is Flags.Case

          if (knownStatically)
            handleStaticallyKnown(tree, scrutinee, selector, inMatch, tree.pos)
          else if (falseIfUnrelated && !(selector <:< scrutinee))
            rewrite(tree, to = false)
          else /*if (happens)*/ tree
        }

        case _ => tree
      }

    evalTypeApply(tree)
  }
}
