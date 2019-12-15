package dotty.tools.dotc
package transform
package init

import MegaPhase._
import ast.tpd
import core._
import Contexts.Context

import scala.collection.mutable


class Checker extends MiniPhase {
  import tpd._

  val phaseName = "initChecker"

  // cache of class summary
  private val baseEnv = Env(null, mutable.Map.empty)

  override val runsAfter = Set(SetDefTree.name)

  override def isEnabled(implicit ctx: Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): tpd.Tree = {
    if (!tree.isClassDef) return tree
    val cls = tree.symbol.asClass

    // A concrete class may not be instantiated if the self type is not satisfied
    if (!cls.isOneOf(Flags.AbstractOrTrait)) {
      implicit val state = Checking.State(
        visited = mutable.Set.empty,
        path = Vector.empty,
        thisClass = cls,
        fieldsInited = mutable.Set.empty,
        parentsInited = mutable.Set.empty,
        env = baseEnv.withCtx(ctx)
      )

      Checking.checkClassBody(tree)
    }

    tree
  }
}
