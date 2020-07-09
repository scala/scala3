package dotty.tools.dotc
package transform
package init


import dotty.tools.dotc._
import ast.tpd

import dotty.tools.dotc.core._
import Contexts.{Context, ctx}
import Types._

import dotty.tools.dotc.transform._
import MegaPhase._


import scala.collection.mutable


class Checker extends MiniPhase {
  import tpd._

  val phaseName = "initChecker"

  // cache of class summary
  private val baseEnv = Env(null, mutable.Map.empty)

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(implicit ctx: Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): tpd.Tree = {
    if (!tree.isClassDef) return tree

    val cls = tree.symbol.asClass
    val instantiable: Boolean =
      cls.is(Flags.Module) ||
      !cls.isOneOf(Flags.AbstractOrTrait) && {
        // see `Checking.checkInstantiable` in typer
        val tp = cls.appliedRef
        val stp = SkolemType(tp)
        val selfType = cls.givenSelfType.asSeenFrom(stp, cls)
        !selfType.exists || stp <:< selfType
      }

    // A concrete class may not be instantiated if the self type is not satisfied
    if (instantiable) {
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
