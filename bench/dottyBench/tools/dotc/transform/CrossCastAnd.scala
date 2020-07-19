package dottyBench.tools.dotc.transform

import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Flags
import dottyBench.tools.dotc.core.Types.AndType
import dottyBench.tools.dotc.transform.MegaPhase._
import tpd._



/**
 * This transform makes sure that all private member selections from
 * AndTypes are performed from the first component of AndType.
 * This is needed for correctness of erasure. See `tests/run/PrivateAnd.scala`
 */
class CrossCastAnd extends MiniPhase {

  override def phaseName: String = "crossCast"

  override def transformSelect(tree: tpd.Select)(using Ctx, CState): tpd.Tree = {

    lazy val qtype = tree.qualifier.tpe.widen
    val sym = tree.symbol
    if (sym.is(Flags.Private) && !sym.isConstructor && qtype.typeSymbol != sym.owner)
      cpy.Select(tree)(tree.qualifier.cast(AndType(qtype.baseType(sym.owner), tree.qualifier.tpe)), tree.name)
    else tree
  }
}
