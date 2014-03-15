package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.Contexts._
import ast.Trees._
import ast.tpd.{Apply, Tree, cpy}

class SamplePhase extends TreeTransformer {

  def init(implicit ctx: Context) = {
    ctx.base.denotTransformers.install(id, new UncurryDenotTransform(_))
  }

  def name = "sample"

  def transformations = Array(new UncurryTreeTransform(_, _))

}

class UncurryDenotTransform(group: DenotTransformerGroup) extends DenotTransformer(group) {

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ???

}

class UncurryTreeTransform(group: TreeTransformer, idx: Int) extends TreeTransform(group, idx) {

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      case Apply(fn, args) => cpy.Apply(tree, fn, args ++ tree.args)
      case _ => tree
    }
}