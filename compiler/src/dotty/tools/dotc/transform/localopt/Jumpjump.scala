package dotty.tools.dotc
package transform.localopt

import core.TypeErasure
import core.Constants.Constant
import core.Contexts.Context
import core.Decorators._
import core.Symbols._
import ast.Trees._
import scala.collection.mutable
import config.Printers.simplify
import core.Flags._

/** Rewrites pairs of consecutive LabelDef jumps by jumping directly to the target.
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class Jumpjump extends Optimisation {
  import ast.tpd._

  val defined = mutable.HashMap[Symbol, Symbol]()

  def clear(): Unit = defined.clear()

  def visitor(implicit ctx: Context): Tree => Unit = {
    case defdef: DefDef if defdef.symbol.is(Label)  =>
      defdef.rhs match {
        case Apply(t, args)
          if t.symbol.is(Label) &&
            TypeErasure.erasure(defdef.symbol.info.finalResultType).classSymbol ==
            TypeErasure.erasure(t.symbol.info.finalResultType).classSymbol             &&
            args.size == defdef.vparamss.map(_.size).sum                               &&
            args.zip(defdef.vparamss.flatten).forall(x => x._1.symbol eq x._2.symbol)  &&
            defdef.symbol != t.symbol                                                  =>

          defined(defdef.symbol) = t.symbol
        case _ =>
      }
    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    case a: Apply if  defined.contains(a.fun.symbol) =>
      defined.get(a.symbol) match {
        case None => a
        case Some(fwd) =>
          ref(fwd).appliedToArgs(a.args)
      }

    case a: DefDef if defined.contains(a.symbol) =>
      simplify.println(s"Dropping ${a.symbol.showFullName} as forwarder to ${defined(a.symbol).showFullName}")
      EmptyTree

    case t => t
  }
}
