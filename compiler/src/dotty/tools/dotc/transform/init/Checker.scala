package dotty.tools.dotc
package transform
package init


import dotty.tools.dotc._
import ast.tpd

import dotty.tools.dotc.core._
import Contexts._
import Types._
import Symbols._

import dotty.tools.dotc.transform._
import MegaPhase._


import scala.collection.mutable


class Checker extends MiniPhase {
  import tpd._

  val phaseName = "initChecker"

  private val semantic = new Semantic

  override val runsAfter = Set(Pickler.name)

  override def isEnabled(using Context): Boolean =
    super.isEnabled && ctx.settings.YcheckInit.value

  override def transformTypeDef(tree: TypeDef)(using Context): tpd.Tree = {
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
    if (instantiable && cls.enclosingPackageClass != defn.StdLibPatchesPackage.moduleClass) {
      import semantic._
      val tpl = tree.rhs.asInstanceOf[Template]
      val thisRef = ThisRef(cls).ensureExists

      val paramValues = tpl.constr.termParamss.flatten.map(param => param.symbol -> Hot).toMap

      given Promoted = Promoted.empty
      given Trace = Trace.empty
      given Env = Env(paramValues)

      val res = eval(tpl, thisRef, cls)
      res.errors.foreach(_.issue)
    }

    tree
  }
}
