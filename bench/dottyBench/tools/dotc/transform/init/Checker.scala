package dottyBench.tools.dotc
package transform
package init


import dottyBench.tools.dotc._
import ast.tpd

import dottyBench.tools.dotc.core._
import Contexts._
import Types._

import dottyBench.tools.dotc.transform._
import MegaPhase._


import scala.collection.mutable


class Checker extends MiniPhase {
  import tpd._

  val phaseName = "initChecker"

  // cache of class summary
  private val baseEnv = Env(null, mutable.Map.empty)

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Ctx): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def transformTypeDef(tree: TypeDef)(using Ctx, CState): tpd.Tree = {
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
      implicit val state: Checking.State = Checking.State(
        visited = mutable.Set.empty,
        path = Vector.empty,
        thisClass = cls,
        fieldsInited = mutable.Set.empty,
        parentsInited = mutable.Set.empty,
        env = baseEnv.withCtx(combinedContext)
      )

      Checking.checkClassBody(tree)
    }

    tree
  }
}
