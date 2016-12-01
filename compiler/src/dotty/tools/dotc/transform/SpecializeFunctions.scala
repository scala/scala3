package dotty.tools.dotc
package transform

import TreeTransforms._
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
import core.Definitions.PerRun
import dotty.tools.dotc.ast.tpd
import collection.{ mutable, immutable }
import collection.mutable.{ LinkedHashMap, LinkedHashSet, TreeSet }

/**
 *  Rewires closures to implement more specific types of Functions.
 */
object SpecializeFunctions {
  import tpd._

  val maxArgsCount = 2

  def shouldSpecialize(paramTypes: List[Type], resultType: Type)(implicit ctx: Context) =
    (paramTypes.size <= maxArgsCount) &&
      paramTypes.forall(x => defn.specializedArgumentTypes.contains(x.typeSymbol)) &&
      defn.specializedReturnTypes.contains(resultType.typeSymbol)

  val functionName = "JFunction".toTermName
  val functionPackage = "scala.compat.java8.".toTermName

  def specialized(formals: List[Type], result: Type)(implicit ctx: Context): Type =
    if (shouldSpecialize(formals, result)) {
      val interfaceName = (functionName ++ formals.length.toString).specializedFor(result :: formals, Nil)
      val interface = ctx.getClassIfDefined(functionPackage ++ interfaceName)
      if (interface.exists) interface.asType.typeRef else NoType
    }
    else NoType

  def specialized(tp: Type)(implicit ctx: Context): Type = tp match {
    case defn.FunctionOf(formals, result) => specialized(formals, result).orElse(tp)
    case _ => tp
  }

  def transformClosure(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: Closure =>
      tree.tpt match {
        case EmptyTree =>
          val mt @ MethodType(_, formals) = tree.meth.tpe.widen
          val specializedType = specialized(formals, mt.resultType)
          if (specializedType.exists) cpy.Closure(tree)(tpt = TypeTree(specializedType))
          else tree
        case _ =>
          tree
      }
    case Block(stats, expr: Closure) =>
      cpy.Block(tree)(stats, transformClosure(expr))
    case _ =>
      tree
  }
}
