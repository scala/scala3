package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core.*
import Contexts.*, Trees.*, Types.*, StdNames.*, Symbols.*
import ValueClasses.*

object TreeExtractors {
  import tpd.*

  /** Match arg1.op(arg2) and extract (arg1, op.symbol, arg2) */
  object BinaryOp {
    def unapply(t: Tree)(using Context): Option[(Tree, Symbol, Tree)] = t match {
      case Apply(sel @ Select(arg1, _), List(arg2)) =>
        Some((arg1, sel.symbol, arg2))
      case _ =>
        None
    }
  }

 /** Match new C(args) and extract (C, args).
  *  Also admit new C(args): T and {new C(args)}.
  */
  object NewWithArgs {
    def unapply(t: Tree)(using Context): Option[(Type, List[Tree])] = t match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), args) =>
        Some((t.tpe, args))
      case Typed(expr, _) => unapply(expr)
      case Block(Nil, expr) => unapply(expr)
      case _ =>
        None
    }
  }

  /** For an instance v of a value class like:
   *    class V(val underlying: X) extends AnyVal
   *  Match v.underlying() and extract v
   */
  object ValueClassUnbox {
    def unapply(t: Tree)(using Context): Option[Tree] = t match {
      case Apply(sel @ Select(ref, _), Nil) =>
        val sym = ref.tpe.widenDealias.typeSymbol
        if (isDerivedValueClass(sym) && (sel.symbol eq valueClassUnbox(sym.asClass)))
          Some(ref)
        else
          None
      case _ =>
        None
    }
  }
}
