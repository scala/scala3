package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types.{NoType, Type, AndType}
import dotty.tools.dotc.transform.TreeTransforms._
import tpd._

import scala.collection.mutable.ListBuffer


/**
 * This transform makes sure that all private member selections from
 * AndTypes are performed from the first component of AndType.
 * This is needed for correctness of erasure. See `tests/run/PrivateAnd.scala`
 */
class CrossCastAnd extends MiniPhaseTransform { thisTransform =>

  override def phaseName: String = "crossCast"

  override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {

    lazy val qtype = tree.qualifier.tpe.widen
    val sym = tree.symbol
    if (sym.is(Flags.Private) && qtype.typeSymbol != sym.owner)
      cpy.Select(tree)(tree.qualifier.asInstance(AndType(qtype.baseTypeWithArgs(sym.owner), tree.qualifier.tpe)), tree.name)
    else tree
  }
}
