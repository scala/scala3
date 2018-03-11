package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.SymDenotations._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import ast.Trees._
import SymUtils._
import dotty.tools.dotc.ast.tpd
import collection.{ mutable, immutable }
import collection.mutable.{ LinkedHashMap, LinkedHashSet, TreeSet }

/**
 *  Rewires closures to implement more specific types of Functions.
 */
class FunctionalInterfaces extends MiniPhase {
  import tpd._

  def phaseName: String = "functionalInterfaces"

  val functionName = "JFunction".toTermName
  val functionPackage = "scala.compat.java8.".toTermName

  override def transformClosure(tree: Closure)(implicit ctx: Context): Tree = {
    val cls = tree.tpe.widen.classSymbol.asClass

    val implType = tree.meth.tpe.widen
    val List(implParamTypes) = implType.paramInfoss
    val implResultType = implType.resultType

    if (defn.isSpecializableFunction(cls, implParamTypes, implResultType)) {
      val names = ctx.atPhase(ctx.erasurePhase) {
        implicit ctx => cls.typeParams.map(_.name)
      }
      val interfaceName = (functionName ++ implParamTypes.length.toString).specializedFor(implParamTypes ::: implResultType :: Nil, names, Nil, Nil)

      // symbols loaded from classpath aren't defined in periods earlier than when they where loaded
      val interface = ctx.withPhase(ctx.typerPhase).requiredClass((functionPackage ++ interfaceName).toTypeName)
      val tpt = tpd.TypeTree(interface.asType.appliedRef)
      tpd.Closure(tree.env, tree.meth, tpt)
    } else tree
  }
}
