package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Names._
import Symbols._
import Contexts._
import Decorators._
import NameOps._
import dotty.tools.dotc.ast.tpd


object FunctionalInterfaces {
  val name: String = "functionalInterfaces"
}

/**
 *  Rewires closures to implement more specific types of Functions.
 */
class FunctionalInterfaces extends MiniPhase {
  import tpd._

  def phaseName: String = FunctionalInterfaces.name

  private[this] val functionName = "JFunction".toTermName
  private[this] val functionPackage = "dotty.runtime.function.".toTermName

  override def transformClosure(tree: Closure)(implicit ctx: Context): Tree = {
    val cls = tree.tpe.widen.classSymbol.asClass

    val implType = tree.meth.tpe.widen
    val List(implParamTypes) = implType.paramInfoss
    val implResultType = implType.resultType

    if (defn.isSpecializableFunction(cls, implParamTypes, implResultType) &&
        !ctx.settings.scalajs.value) { // never do anything for Scala.js, but do this test as late as possible not to slow down Scala/JVM
      val names = ctx.atPhase(ctx.erasurePhase) {
        implicit ctx => cls.typeParams.map(_.name)
      }
      val interfaceName = (functionName ++ implParamTypes.length.toString).specializedFor(implParamTypes ::: implResultType :: Nil, names, Nil, Nil)

      // symbols loaded from classpath aren't defined in periods earlier than when they where loaded
      val interface = ctx.withPhase(ctx.typerPhase).requiredClass(functionPackage ++ interfaceName)
      val tpt = tpd.TypeTree(interface.asType.appliedRef)
      tpd.Closure(tree.env, tree.meth, tpt)
    } else tree
  }
}
