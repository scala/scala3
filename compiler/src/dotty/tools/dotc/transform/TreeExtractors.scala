package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Trees._, Types._, StdNames._, Symbols._
import util.Result
import ValueClasses._

object TreeExtractors {
  import tpd._

  /** Match arg1.op(arg2) and extract (arg1, op.symbol, arg2) */
  object BinaryOp {
    def unapply(t: Tree)(implicit ctx: Context): Result[(Tree, Symbol, Tree)] = t match {
      case Apply(sel @ Select(arg1, _), List(arg2)) =>
        Result((arg1, sel.symbol, arg2))
      case _ =>
        Result.empty
    }
  }

 /** Match new C(args) and extract (C, args) */
  object NewWithArgs {
    def unapply(t: Tree)(implicit ctx: Context): Result[(Type, List[Tree])] = t match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), args) =>
        Result((t.tpe, args))
      case _ =>
        Result.empty
    }
  }

  /** For an instance v of a value class like:
   *    class V(val underlying: X) extends AnyVal
   *  Match v.underlying() and extract v
   */
  object ValueClassUnbox {
    def unapply(t: Tree)(implicit ctx: Context): Result[Tree] = t match {
      case Apply(sel @ Select(ref, _), Nil) =>
        val sym = ref.tpe.widenDealias.typeSymbol
        if (isDerivedValueClass(sym) && (sel.symbol eq valueClassUnbox(sym.asClass))) {
          Result(ref)
        } else
          Result.empty
      case _ =>
        Result.empty
    }
  }
}
