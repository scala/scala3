package dotty.tools.dotc
package transform

import core._
import Names._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import Flags._
import Contexts.Context
import Symbols._
import Constants._
import Decorators._
import Denotations._, SymDenotations._
import Decorators.StringInterpolators
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import scala.collection.mutable
import DenotTransformers._
import Names.Name
import NameOps._
import TypeUtils._

/** A transformer that removes effect value parameters and effect argument terms.
 *  Effect type parameters and arguments are not affected.
 */
class ElimEffects extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName = "elimEffects"

  override def relaxedTypingInGroup: Boolean = true // because it drops effect parameters

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimEffects(tp)

  private def elimEffects(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      tp.derivedLambdaType(paramNames, paramTypes.filterNot(_.isEffect), elimEffects(resultType))
    case tp =>
      tp
  }

  private def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimEffects(tree.tpe))

  private def elimIfEffect(tree: Tree)(implicit ctx: Context): Tree =
    if (tree.tpe.isEffect && !tree.symbol.is(Param)) EmptyTree else tree

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    cpy.Apply(tree)(tree.fun, tree.args.filterNot(_.tpe.isEffect))

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    elimIfEffect(tree)

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree =
    elimIfEffect(cpy.DefDef(tree)(vparamss = tree.vparamss.map(_.filterNot(_.symbol.info.isEffect))))
}
