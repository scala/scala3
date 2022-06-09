package dotty.tools.dotc
package transform

import core._
import Constants.Constant
import Contexts._
import Flags._
import Definitions._
import DenotTransformers._
import StdNames._
import Symbols._
import MegaPhase._
import Types._


/** This phase adds forwarder for XXL functions `apply` methods that are implemented with a method
 *  with explicit parameters (not in Array[Object]).
 *
 *  In particular for every method
 *    `def apply(x1: T1, ... xn: Tn): R` in class `M` subtype of `FunctionN[T1, ..., Tn, R]` with `N` > 22
 *  a forwarder
 *    `def apply(xs: Array[Object]): R = this.apply(xs(0).asInstanceOf[T1], ..., xs(n-1).asInstanceOf[Tn]).asInstanceOf[R]`
 *  is generated.
 */
class FunctionXXLForwarders extends MiniPhase with IdentityDenotTransformer {
  import ast.tpd._

  override def phaseName: String = FunctionXXLForwarders.name

  override def description: String = FunctionXXLForwarders.description

  override def transformTemplate(impl: Template)(using Context): Template = {

    def forwarderRhs(receiver: Tree, xsTree: Tree): Tree = {
      val argsApply = ref(xsTree.symbol).select(nme.apply)
      var idx = -1
      val argss = receiver.tpe.widenDealias.paramInfoss.map(_.map { param =>
        idx += 1
        argsApply.appliedToTermArgs(List(Literal(Constant(idx)))).cast(param)
      })
      ref(receiver.symbol).appliedToArgss(argss).cast(defn.ObjectType)
    }

    if impl.symbol.owner.is(Trait) then return impl

    val forwarders =
      for {
        case (ddef: DefDef) <- impl.body
        if ddef.name == nme.apply && ddef.symbol.is(Method) &&
           ddef.symbol.signature.paramsSig.size > MaxImplementedFunctionArity &&
           ddef.symbol.allOverriddenSymbols.exists(sym => defn.isXXLFunctionClass(sym.owner))
      }
      yield {
        val xsType = defn.ArrayType.appliedTo(List(defn.ObjectType))
        val methType = MethodType(List(nme.args))(_ => List(xsType), _ => defn.ObjectType)
        val meth = newSymbol(ddef.symbol.owner, nme.apply, Synthetic | Method, methType)
        DefDef(meth, paramss => forwarderRhs(ddef, paramss.head.head))
      }

    cpy.Template(impl)(body = forwarders ::: impl.body)
  }
}

object FunctionXXLForwarders:
  val name: String = "functionXXLForwarders"
  val description: String = "add forwarders for FunctionXXL apply methods"
