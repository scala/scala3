package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomTypeErasure extends MiniPhaseTransform with InfoTransformer {

  import tpd._

  override def phaseName: String = "phantomTypeErasure"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    assert(!tree.tpe.isPhantom, tree.tpe + " should be erased in " + tree)
  }

  /* Tree transform */

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val newTpe = erasePhantomAnyType(tree.tpe)
    if (newTpe =:= tree.tpe) tree else TypeTree(newTpe)
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (defn.isPhantomAssume(tree.fun.symbol)) Literal(Constant(null)).withType(defn.ErasedPhantomType) else tree

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasePhantomAnyType(tp)

  /* private methods */

  private def erasePhantomAnyType(tp: Type)(implicit ctx: Context): Type = {
    val erasePhantomAnyTypeMap = new DeepTypeMap {
      override def apply(tp: Type): Type = tp match {
        case tp: TypeRef if defn.isPhantomAnyClass(tp.symbol) || defn.isPhantomNothingClass(tp.symbol) =>
          defn.ErasedPhantomType
        case tp: TypeRef if tp.typeSymbol eq defn.PhantomClass =>
          defn.ErasedPhantomLatticeType
        case tp: MethodType if tp.resultType.isPhantom =>
          // Erase return type to Object to match FunctionN erased return type
          val methodType = if (tp.isImplicit) ImplicitMethodType else MethodType
          methodType(tp.paramNames, tp.paramInfos, defn.ObjectType)
        case _ => mapOver(tp)
      }
    }
    erasePhantomAnyTypeMap(tp)
  }

}
