package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types.AndType
import dotty.tools.dotc.transform.MegaPhase._
import tpd._
import dotty.tools.dotc.core.Decorators._

/**
 * This transform makes sure that all private member selections from
 * AndTypes are performed from the first component of AndType.
 * This is needed for correctness of erasure. See `tests/run/PrivateAnd.scala`
 */
class CrossCastAnd extends MiniPhase {

  override def phaseName: String = "crossCast"

  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = {

    lazy val qtype = tree.qualifier.tpe.widen
    val sym = tree.symbol
    if sym.is(Flags.Private)
       && !sym.isConstructor
       && qtype.classSymbol != sym.owner
    then
      cpy.Select(tree)(tree.qualifier.cast(AndType(qtype.baseType(sym.owner), tree.qualifier.tpe)), tree.name)
    else tree
  }
}
