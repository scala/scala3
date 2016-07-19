package dotty.tools.dotc.transform.phantom

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.DenotTransformers._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

class PhantomTermErasure extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "phantomTermErasure"

  /** Check what the phase achieves, to be called at any point after it is finished. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def assertNotPhantom(tree: Tree): Unit =
      assert(!tree.tpe.isPhantom, "All phantom type values should be erased in " + tree)
    tree match {
      case Apply(_, args) => args.foreach(assertNotPhantom)
      case DefDef(_, _, vparamss, tpt, _) => vparamss.foreach(_.foreach(assertNotPhantom))
      case _ =>
    }
  }

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    cpy.Apply(tree)(tree.fun, tree.args.filter(!_.tpe.isPhantom))

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    cpy.DefDef(ddef)(vparamss = ddef.vparamss.map(_.filter(!_.tpt.typeOpt.isPhantom)))

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.is(Param) && tree.tpe.isPhantom) Literal(Constant(null)).withType(tree.tpe) else tree

  /* Symbol transform */

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = erasedPhantomParameters(tp)

  /* private methods */

  private def erasedPhantomParameters(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: JavaMethodType => tp
    case tp: MethodType =>
      val methodType = if (tp.isImplicit) ImplicitMethodType else MethodType
      val (erasedParamNames, erasedParamTypes) =
        tp.paramNames.zip(tp.paramTypes).filterNot(_._2.isPhantom).unzip
      val erasedReturnType = erasedPhantomParameters(tp.resultType)
      methodType(erasedParamNames, erasedParamTypes, erasedReturnType)
    case tp: PolyType =>
      val erasedReturnType = erasedPhantomParameters(tp.resultType)
      tp.derivedPolyType(tp.paramNames, tp.paramBounds, erasedReturnType)
    case _            => tp
  }
}
