package dotty.tools.dotc
package transform

import core._
import Names._
import Types._
import TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import ast.Trees.flatten
import Flags._
import Contexts.Context
import Symbols._
import Denotations._, SymDenotations._
import Decorators.StringInterpolators
import scala.collection.mutable
import DenotTransformers._
import Names.Name
import NameOps._
import TypeUtils._

/** A transformer that removes repeated parameters (T*) from all types, replacing
 *  them with Seq types.
 */
class ElimRepeated extends TreeTransform with InfoTransformer { thisTransformer =>
  import ast.tpd._

  override def name = "elimrepeated"

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimRepeated(tp)

  private def elimRepeated(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
    case tp @ MethodType(paramNames, paramTypes) =>
      val resultType1 = elimRepeated(tp.resultType)
      val paramTypes1 =
        if (paramTypes.nonEmpty && paramTypes.last.isRepeatedParam) {
          paramTypes.init :+ paramTypes.last.repeatedToSeq
        }
        else paramTypes
      tp.derivedMethodType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, elimRepeated(tp.resultType))
    case tp =>
      tp
  }

  def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimRepeated(tree.tpe))

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
}
