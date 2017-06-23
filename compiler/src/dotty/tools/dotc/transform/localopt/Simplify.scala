package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.DenotTransformers.IdentityDenotTransformer
import core.Symbols._
import core.Types._
import core.Flags._
import core.Decorators._
import core.NameOps._
import transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import config.Printers.simplify
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

  private[localopt] var SeqFactoryClass: Symbol = null
  private[localopt] var CommutativePrimitiveOperations: Set[Symbol] = null

  /** The original intention is to run most optimizations both before and after erasure.
   *  Erasure creates new inefficiencies as well as new optimization opportunities.
   *
   *  The order of optimizations is tuned to converge faster.
   *  Reordering them may require quadratically more rounds to finish.
   */
  private def beforeErasure: List[Optimisation] =
    new InlineCaseIntrinsics(this)  ::
    new RemoveUnnecessaryNullChecks ::
    new InlineOptions               ::
    new InlineLabelsCalledOnce      ::
    new Valify(this)                ::
    new Devalify                    ::
    new Jumpjump                    ::
    new DropGoodCasts               ::
    new DropNoEffects(this)         ::
    new InlineLocalObjects(this)    ::
    // new Varify                      :: // varify could stop other transformations from being applied. postponed.
    // new BubbleUpNothing             ::
    new ConstantFold(this)          ::
    Nil

  /** See comment on beforeErasure */
  private def afterErasure: List[Optimisation] =
    new Valify(this)                ::
    new Devalify                    ::
    new Jumpjump                    ::
    new DropGoodCasts               ::
    new ConstantFold(this)          ::
    Nil

  var optimisations: List[Optimisation] = Nil

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
    SeqFactoryClass = ctx.requiredClass("scala.collection.generic.SeqFactory")
    CommutativePrimitiveOperations = Set(defn.Boolean_&&, defn.Boolean_||, defn.Int_+, defn.Int_*, defn.Long_+, defn.Long_*)

    val maxFuel = ctx.settings.YoptFuel.value
    if (fuel < 0 && maxFuel > 0) // Both defaults are at -1
      fuel = maxFuel

    optimisations = {
      val o = if (ctx.erasedTypes) afterErasure else beforeErasure
      val p = ctx.settings.YoptPhases.value
      if (p.isEmpty) o else o.filter(x => p.contains(x.name))
    }

    this
  }

  // The entry point of local optimisation: DefDefs
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val ctx0 = ctx
    if (ctx.settings.optimise.value && !tree.symbol.is(Label)) {
      implicit val ctx: Context = ctx0.withOwner(tree.symbol(ctx0))
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
              printIfDifferent(childOptimizedTree, optimisation.transformer(ctx)(childOptimizedTree), optimisation)
            }
          }.transform(rhs0)

          // Clean
          optimisation.clear()
        }
      }
      if (rhs0 ne tree.rhs) tpd.cpy.DefDef(tree)(rhs = rhs0)
      else tree
    } else tree
  }

  private def printIfDifferent(tree1: Tree, tree2: => Tree, opt: Optimisation)(implicit ctx: Context): Tree = {
    if (fuel == -1)
      tree2 // Does nothing when fuel is disabled.
    else if (fuel == 0)
      tree1 // No more fuel? No more transformations for you!
    else {  // Print the trees if different and consume fuel accordingly.
      if (tree1 ne tree2) {
        if (fuel > 0) fuel -= 1
        if (fuel != -1) {
          println(s"${tree1.symbol} was simplified by ${opt.name} (fuel=$fuel): ${tree1.show}")
          println(s"became after ${opt.name}: (fuel=$fuel) ${tree2.show}")
        }
      }
      tree2
    }
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

  /** Is this tree mutable, or java.lang.System.{in, out, err}? These three
   *  System members are the only static final fields that are mutable.
   *  See https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html#jls-17.5.4
   */
  def isEffectivelyMutable(t: Tree)(implicit ctx: Context): Boolean = t match {
    case _ if t.symbol.is(Mutable) => true
    case s: Select => s.symbol.owner == defn.SystemModule
    case i: Ident  => desugarIdent(i).exists(isEffectivelyMutable)
    case _ => false
  }

  def isImmutableAccessor(t: Tree)(implicit ctx: Context): Boolean = {
    val isImmutableGetter = t.symbol.isGetter && !t.symbol.is(Mutable | Lazy)
    val isCaseAccessor    = t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable | Lazy)
    val isProductAccessor = t.symbol.exists                               &&
                            t.symbol.owner.derivesFrom(defn.ProductClass) &&
                            t.symbol.owner.is(CaseClass)                  &&
                            t.symbol.name.isSelectorName                  &&
                            !t.symbol.info.decls.exists(_.is(Mutable | Lazy)) // Conservatively covers case class A(var x: Int)
    isImmutableGetter || isCaseAccessor || isProductAccessor
  }
}
