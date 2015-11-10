package dotty.tools.dotc
package transform

import ast.tpd
import core.Constants.Constant
import core.Contexts.Context
import core.StdNames.nme
import core.Symbols.{defn,TermSymbol}
import core.TypeErasure
import TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}

/** Rewrite `classOf` calls as follow:
 *
 *  For every primitive class C whose boxed class is called B:
 *    classOf[C]    -> B.TYPE
 *  For every non-primitive class D:
 *    classOf[D]    -> Literal(Constant(erasure(D)))
 */
class ClassOf extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "classOf"

  private var classOfMethod: TermSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    classOfMethod = defn.ScalaPredefModule.requiredMethod(nme.classOf)
    this
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol eq classOfMethod) {
      val targ = tree.args.head.tpe
      tree.clsOf(targ, Literal(Constant(TypeErasure.erasure(targ))))
    }
    else tree
}
