package dotty.tools
package dotc
package transform

import MegaPhase.MiniPhase
import core.*
import Symbols.*, Contexts.*, Types.*, Decorators.*
import StdNames.nme
import SymUtils.*
import NameKinds.AdaptedClosureName

/** Rewrite `(x1, ... xN) => f(x1, ... xN)` for N >= 0 to `f`,
 *  provided `f` is a pure path of function type.
 *
 *  This optimization is crucial for context functions. The compiler
 *  produces a contextual closure around values passed as arguments
 *  where a context function is expected, unless that value has the
 *  syntactic form of a context function literal.
 *
 *  Also handle variants of eta-expansions where
 *   - result f.apply(X_1,...,X_n) is subject to a synthetic cast, or
 *   - the application uses a specialized apply method, or
 *   - the closure is adapted (see Erasure#adaptClosure)
 *
 *  Without this phase, when a contextual function is passed as an argument to a
 *  recursive function, that would have the unfortunate effect of a linear growth
 *  in transient thunks of identical type wrapped around each other, leading
 *  to performance degradation, and in some cases, stack overflows.
 */
class EtaReduce extends MiniPhase:
  import ast.tpd._

  override def phaseName: String = EtaReduce.name

  override def description: String = EtaReduce.description

  override def transformBlock(tree: Block)(using Context): Tree =

    def tryReduce(mdef: DefDef, rhs: Tree): Tree = rhs match
      case Apply(Select(fn, name), args)
      if (name == nme.apply || defn.FunctionSpecializedApplyNames.contains(name))
          && mdef.paramss.head.corresponds(args)((param, arg) =>
              arg.isInstanceOf[Ident] && arg.symbol == param.symbol)
          && isPurePath(fn)
          && fn.tpe <:< tree.tpe
          && defn.isFunctionClass(fn.tpe.widen.typeSymbol) =>
        report.log(i"eta reducing $tree --> $fn")
        fn
      case TypeApply(Select(qual, _), _) if rhs.symbol.isTypeCast && rhs.span.isSynthetic =>
        tryReduce(mdef, qual)
      case Apply(_, arg :: Nil) if Erasure.Boxing.isUnbox(rhs.symbol) && rhs.span.isSynthetic =>
        tryReduce(mdef, arg)
      case Block(call :: Nil, unit @ Literal(Constants.Constant(()))) if unit.span.isSynthetic =>
        tryReduce(mdef, call)
      case _ =>
        tree

    tree match
      case Block((meth: DefDef) :: Nil, expr) if meth.symbol.isAnonymousFunction =>
        expr match
          case closure: Closure if meth.symbol == closure.meth.symbol =>
            tryReduce(meth, meth.rhs)
          case Block((adapted: DefDef) :: Nil, closure: Closure)
          if adapted.name.is(AdaptedClosureName) && adapted.symbol == closure.meth.symbol =>
            tryReduce(meth, meth.rhs)
          case _ =>
            tree
      case _ =>
        tree
  end transformBlock

end EtaReduce

object EtaReduce:
  val name: String = "etaReduce"
  val description: String = "reduce eta expansions of pure paths"
