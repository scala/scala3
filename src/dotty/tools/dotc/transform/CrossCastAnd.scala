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
    if (tree.symbol.is(Flags.Private)) {
      // all private member selections have a symbol
      tree.qualifier.tpe.widen match {
        case t @ AndType(l, _) =>

        // find a component of and type that owns the symbol
        def findType(tp: Type): Type = {
            tp match {
              case AndType(l, r) =>
                findType(l).orElse(findType(r))
              case t =>
                if (t.decl(tree.symbol.name).suchThat(_ == tree.symbol).exists)
                  t
                else NoType
            }
          }

          val tp = findType(t)
          if (l eq tp) tree
          else tree.qualifier.asInstance(AndType(tp, t)).select(tree.symbol)
        case _ => tree
      }
    }
      else tree
  }
}
