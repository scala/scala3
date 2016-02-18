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

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol eq defn.Predef_classOf) {
      val targ = tree.args.head.tpe
      clsOf(targ).ensureConforms(tree.tpe).withPos(tree.pos)
    }
    else tree
}
