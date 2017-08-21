package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Contexts._
import ast.tpd
import dotty.tools.dotc.core.Phases

/** This phase transforms wildcards in valdefs with their default value.
  *  In particular for every valdef that is declared:
  *    `val x : T = _` to `val x : T = <zero of T>`
  *
  */
class TransformWildcards extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import tpd._

  override def phaseName = "transformWildcards"

  override def runsAfter: Set[Class[_ <: Phases.Phase]] = Set(classOf[LiftedClasses])

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case vDef: ValDef => assert(!tpd.isWildcardArg(vDef.rhs))
      case _ =>
    }
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (ctx.owner.isClass) tree
    else cpy.ValDef(tree)(rhs = tree.rhs.wildcardToDefault)
  }
}
