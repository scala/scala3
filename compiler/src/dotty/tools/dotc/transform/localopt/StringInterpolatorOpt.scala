package dotty.tools.dotc.transform.localopt

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

/**
  * Created by wojtekswiderski on 2018-01-24.
  */
class StringInterpolatorOpt extends MiniPhase {
  import tpd._

  override def phaseName: String = "stringInterpolatorOpt"

  private object StringContextIntrinsic {
    def unapply(tree: Apply)(implicit ctx: Context): Option[(List[Tree], List[Tree])] = {
      tree match {
        case Apply(Select(Apply(Select(Ident(nme.StringContext), nme.apply),
                   List(SeqLiteral(strs, _))), id), List(SeqLiteral(elems, _))) =>
          if (id == nme.raw_) Some(strs, elems)
          else if (id == nme.s) {
            try {
              val escapedStrs = strs.mapConserve { str =>
                val strValue = str.asInstanceOf[Literal].const.stringValue
                val escapedValue = StringContext.processEscapes(strValue)
                cpy.Literal(str)(Constant(escapedValue))
              }
              Some(escapedStrs, elems)
            } catch {
              case _: StringContext.InvalidEscapeException => None
            }
          } else None
        case _ => None
      }
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    tree match {
      case StringContextIntrinsic(strs: List[Tree], elems: List[Tree]) =>
        val numLits = strs.length
        strs.tail.foldLeft((0, strs.head)) { (acc: (Int, Tree), str: Tree) =>
          val (i, result) = acc
          val resultWithElem =
            if (i < numLits - 1) result.select(defn.String_+).appliedTo(elems(i))
            else result
          val resultWithStr =
            if (str.asInstanceOf[Literal].const.stringValue.isEmpty) resultWithElem
            else resultWithElem.select(defn.String_+).appliedTo(str)
          (i + 1, resultWithStr)
        }._2
      case _ => tree
    }
  }
}
