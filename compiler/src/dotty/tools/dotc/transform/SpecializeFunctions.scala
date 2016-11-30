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
class SpecializeFunctions {
  import tpd._

  val maxArgsCount = 2

  val specializedReturnTypesPerRun = new PerRun[Set[Symbol]](implicit ctx => Set(
                                 defn.UnitClass,
                                 defn.BooleanClass,
                                 defn.IntClass,
                                 defn.LongClass,
                                 defn.FloatClass,
                                 defn.DoubleClass,
     /* only for Function0: */   defn.ByteClass,
                                 defn.ShortClass,
                                 defn.CharClass))
  def specializedReturnTypes(implicit ctx: Context) = specializedReturnTypesPerRun()(ctx)

  val specializedArgumentTypesPerRun = new PerRun[Set[Symbol]](implicit ctx => Set(
                                 defn.IntClass,
                                 defn.LongClass,
                                 defn.FloatClass,
                                 defn.DoubleClass))
  def specializedArgumentTypes(implicit ctx: Context) = specializedArgumentTypesPerRun()(ctx)

  def shouldSpecialize(m: MethodType)(implicit ctx: Context) =
    (m.paramTypes.size <= maxArgsCount) &&
      m.paramTypes.forall(x => specializedArgumentTypes.contains(x.typeSymbol)) &&
      specializedReturnTypes.contains(m.resultType.typeSymbol)

  val functionName = "JFunction".toTermName
  val functionPackage = "scala.compat.java8.".toTermName

  def transformClosure(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: Closure =>
      tree.tpt match {
        case EmptyTree =>
          val m = tree.meth.tpe.widen.asInstanceOf[MethodType]

          if (shouldSpecialize(m)) {
            val functionSymbol = tree.tpe.widenDealias.classSymbol
            val names = ctx.atPhase(ctx.erasurePhase) {
              implicit ctx => functionSymbol.typeParams.map(_.name)
            }
            val interfaceName = (functionName ++ m.paramTypes.length.toString).specializedFor(m.paramTypes ::: m.resultType :: Nil, names, Nil, Nil)

            // symbols loaded from classpath aren't defined in periods earlier than when they where loaded
            val interface = ctx.withPhase(ctx.typerPhase).getClassIfDefined(functionPackage ++ interfaceName)
            if (interface.exists) {
              val tpt = tpd.TypeTree(interface.asType.typeRef)
              Closure(tree.env, tree.meth, tpt)
            } else tree
          } else tree
        case _ =>
          tree
      }
    case Block(stats, expr: Closure) =>
      cpy.Block(tree)(stats, transformClosure(expr))
    case _ =>
      tree
  }
}
