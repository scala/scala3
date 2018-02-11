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
        case Apply(Select(Apply(Select(ident, nme.apply), List(SeqLiteral(strs, _))), fn),
            List(SeqLiteral(elems, _))) =>
          val clsSym = ident.symbol.companionClass
          if (clsSym.eq(defn.StringContextClass) && strs.forall(_.isInstanceOf[Literal])) {
            if (fn == nme.raw_) Some(strs, elems)
            else if (fn == nme.s) {
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
          } else None
        case _ => None
      }
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    tree match {
      case StringContextIntrinsic(strs: List[Tree], elems: List[Tree]) =>
        val stri = strs.iterator
        val elemi = elems.iterator
        var result = stri.next
        def concat(tree: Tree): Unit = {
          result = result.select(defn.String_+).appliedTo(tree)
        }
        while (elemi.hasNext) {
          concat(elemi.next)
          val str = stri.next
          if (!str.asInstanceOf[Literal].const.stringValue.isEmpty) concat(str)
        }
        result
      case _ => tree
    }
  }
}
