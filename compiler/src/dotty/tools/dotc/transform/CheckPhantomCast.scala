package dotty.tools.dotc
package transform

import core._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd


/** A no-op transform to ensure that the compiled sources have no Phantom types in casts */
class CheckPhantomCast extends MiniPhaseTransform { thisTransformer =>

  override def phaseName = "checkPhantomCast"

  override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (tree.fun.symbol eq defn.Any_asInstanceOf)
      checkNoPhantoms(tree.args.head)
    tree
  }

  override def transformBind(tree: tpd.Bind)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    tree.body match {
      case Typed(_, tpt) => checkNoPhantoms(tpt)
      case _ =>
    }
    tree
  }

  private def checkNoPhantoms(tpTree: tpd.Tree)(implicit ctx: Context): Unit = {
    val checker = new TypeTraverser() {
      override def traverse(tp: Type): Unit = {
        if (tp.isPhantom) ctx.error("Cannot cast type containing a phantom type", tpTree.pos)
        else traverseChildren(tp)
      }
    }
    checker.traverse(tpTree.tpe)
  }

}
