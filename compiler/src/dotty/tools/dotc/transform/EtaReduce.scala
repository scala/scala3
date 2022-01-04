package dotty.tools
package dotc
package transform

import MegaPhase.MiniPhase
import core.*
import Symbols.*, Contexts.*, Types.*, Decorators.*
import StdNames.nme
import ast.Trees.*

/** Rewrite `(x1, ... xN) => f(x1, ... xN)` for N >= 0 to `f`,
 *  provided `f` is a pure path of function type.
 *
 *  This optimization is crucial for context functions. The compiler
 *  produces a contextual closure around values passed as arguments
 *  where a context function is expected, unless that value has the
 *  syntactic form of a context function literal.
 *
 *  Without this phase, when a contextual function is passed as an argument to a
 *  recursive function, that would have the unfortunate effect of a linear growth
 *  in transient thunks of identical type wrapped around each other, leading
 *  to performance degradation, and in some cases, stack overflows.
 */
class EtaReduce extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = "etaReduce"

  override def transformBlock(tree: Block)(using Context): Tree = tree match
    case Block((meth : DefDef) :: Nil, closure: Closure)
    if meth.symbol == closure.meth.symbol =>
      meth.rhs match
        case Apply(Select(fn, nme.apply), args)
        if meth.paramss.head.corresponds(args)((param, arg) =>
              arg.isInstanceOf[Ident] && arg.symbol == param.symbol)
            && isPurePath(fn) =>
          val treeSym = tree.tpe.widen.typeSymbol
          val fnSym = fn.tpe.widen.typeSymbol
          if treeSym == fnSym && defn.isFunctionClass(fnSym) then
            report.log(i"eta reducing $tree --> $fn")
            fn
          else tree
        case _ => tree
    case _ => tree

end EtaReduce