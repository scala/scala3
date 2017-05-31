package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.DenotTransformers.IdentityDenotTransformer
import core.Types._
import transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import config.Printers.simplify
import core.Flags._
import ast.tpd

/** This phase consists of a series of small, simple, local optimisations
 *  applied as a fix point transformation over Dotty Trees.
 *
 *  The termination condition uses referential equality on Trees. Furthermore,
 *  termination relies of every optimisation to be shrinking transformations.
 */
class Simplify extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._
  override def phaseName: String = "simplify"
  override val cpy = tpd.cpy

  def beforeErasure(implicit ctx: Context): List[Optimisation] =
    new InlineCaseIntrinsics        ::
    new RemoveUnnecessaryNullChecks ::
    new InlineOptions               ::
    new InlineLabelsCalledOnce      ::
    new Valify(this)                ::
    new Devalify                    ::
    new Jumpjump                    ::
    new DropGoodCasts               ::
    new DropNoEffects(this)         ::
    // new InlineLocalObjects          :: // followCases needs to be fixed, see ./tests/pos/rbtree.scala
    // new Varify                      :: // varify could stop other transformations from being applied. postponed.
    // new BubbleUpNothing             ::
    new ConstantFold                ::
    Nil

  def afterErasure(implicit ctx: Context): List[Optimisation] =
    // new InlineCaseIntrinsics        ::
    // new RemoveUnnecessaryNullChecks ::
    // new InlineOptions               ::
    // new InlineLabelsCalledOnce      ::
    new Valify(this)                ::
    new Devalify                    ::
    new Jumpjump                    ::
    new DropGoodCasts               ::
    // new DropNoEffects(this)         ::
    // new InlineLocalObjects          :: // followCases needs to be fixed, see ./tests/pos/rbtree.scala
    // new Varify                      :: // varify could stop other transformations from being applied. postponed.
    // new BubbleUpNothing             ::
    new ConstantFold                ::
    Nil

  // The entry point of local optimisation: DefDefs
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val ctx0 = ctx
    if (ctx.settings.optimise.value && !tree.symbol.is(Label)) {
      implicit val ctx: Context = ctx0.withOwner(tree.symbol(ctx0))
      val optimisations = if (ctx.erasedTypes) afterErasure else beforeErasure

      var rhs0 = tree.rhs
      var rhs1: Tree = null
      while (rhs1 ne rhs0) {
        rhs1 = rhs0
        val context = ctx.withOwner(tree.symbol)
        // TODO: fuse for performance
        optimisations.foreach { optimisation =>
          rhs0.foreachSubTree(optimisation.visitor)

          val rhst = new TreeMap() {
            override def transform(tree: Tree)(implicit ctx: Context): Tree = {
              val innerCtx = if (tree.isDef && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
              optimisation.transformer(ctx)(super.transform(tree)(innerCtx))
            }
          }.transform(rhs0)

          if (rhst ne rhs0) {
            simplify.println(s"${tree.symbol} was simplified by ${optimisation.name}: ${rhs0.show}")
            simplify.println(s"became: ${rhst.show}")
          }
          rhs0 = rhst
        }
      }
      if (rhs0 ne tree.rhs) tpd.cpy.DefDef(tree)(rhs = rhs0)
      else tree
    } else tree
  }
}

object Simplify {
  import tpd._
  def desugarIdent(i: Ident)(implicit ctx: Context): Option[Select] = {
    i.tpe match {
      case TermRef(prefix: TermRef, name) =>
        Some(ref(prefix).select(i.symbol))
      case TermRef(prefix: ThisType, name) =>
        Some(This(prefix.cls).select(i.symbol))
      case _ => None
    }
  }
}
