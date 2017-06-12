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
 *
 *  This phase is intended to be run multiple times in the compilation pipeline.
 *  This is due to several reasons:
 *   - running this phase early allows to reduce size of compilation unit, speeding up subsequent transformations.
 *   - running this phase late allows to eliminate inefficiencies created by previous phase
 *   - different patters are easier to optimize at different moments of pipeline
 */
class Simplify extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._
  override def phaseName: String = "simplify"
  override val cpy = tpd.cpy

  /** The original intention is to run most optimizations both before and after erasure.
   * Erasure creates new inefficiencies as well as new optimization opportunities.
   *
   * The order of optimizations is tuned to converge faster.
   * Reordering them may require quadratically more rounds to finish.
   */
  private val beforeErasure: List[Optimisation] =
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

  /** see comment on  beforeErasure */
  private val afterErasure: List[Optimisation] =
    new Valify(this)                ::
    new Devalify                    ::
    new Jumpjump                    ::
    new DropGoodCasts               ::
    new ConstantFold                ::
    Nil

  /** Optimisation fuel, for debugging. Decremented every time Simplify
   *  applies an optimisation until fuel == 0. Original idea from Automatic
   *  Isolation of Compiler Errors by David Whalley. Unable with -Yopt-fuel.
   *
   *  The fuel can be used to do a bisection on large test cases that fail
   *  -optimise. See compiler/test/bisect.sh for a shell script to automates
   *  the bisection search.
   */
  var fuel: Int = -1

  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    val maxFuel = ctx.settings.YoptFuel.value
    if (fuel < 0 && maxFuel > 0) // Both defaults are at -1
      fuel = maxFuel
    this
  }

  // The entry point of local optimisation: DefDefs
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val ctx0 = ctx
    if (ctx.settings.optimise.value && !tree.symbol.is(Label)) {
      implicit val ctx: Context = ctx0.withOwner(tree.symbol(ctx0))
      val optimisations = {
        val o = if (ctx.erasedTypes) afterErasure else beforeErasure
        val p = ctx.settings.YoptPhases.value
        if (p.isEmpty) o else o.filter(x => p.contains(x.name))
      }

      var rhs0 = tree.rhs
      var rhs1: Tree = null
      while (rhs1 ne rhs0) {
        rhs1 = rhs0
        val context = ctx.withOwner(tree.symbol)
        optimisations.foreach { optimisation => // TODO: fuse for performance
          // Visit
          rhs0.foreachSubTree(optimisation.visitor)

          // Transform
          rhs0 = new TreeMap() {
            override def transform(tree: Tree)(implicit ctx: Context): Tree = {
              val innerCtx = if (tree.isDef && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
              val childOptimizedTree = super.transform(tree)(innerCtx)

              if (fuel == 0)
                childOptimizedTree
              else {
                val fullyOptimizedTree = optimisation.transformer(ctx)(childOptimizedTree)

                if (tree ne fullyOptimizedTree) {
                  if (fuel > 0) fuel -= 1
                  if (fuel != -1 && fuel < 10) {
                    println(s"${tree.symbol} was simplified by ${optimisation.name} (fuel=$fuel): ${tree.show}")
                    println(s"became after ${optimisation.name}: (fuel=$fuel) ${fullyOptimizedTree.show}")
                  }
                }
                fullyOptimizedTree
              }
            }
          }.transform(rhs0)
        }
      }
      if (rhs0 ne tree.rhs) tpd.cpy.DefDef(tree)(rhs = rhs0)
      else tree
    } else tree
  }
}

object Simplify {
  import tpd._
  // TODO: This function is duplicated in jvm/DottyBackendInterface.scala, let's factor these out!
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
