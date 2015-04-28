package dotty.tools.dotc
package transform

import core._
import ast.Trees._
import Contexts._, Types._, Symbols._, Flags._, TypeUtils._, DenotTransformers._, StdNames._

/** For all parameter accessors
 *
 *      val x: T = ...
 *
 *  if
 *  (1) x is forwarded in the supercall to a parameter that's also named `x`
 *  (2) the superclass parameter accessor for `x` is accessible from the current class to
 *  change the accessor to
 *
 *      def x: T = super.x.asInstanceOf[T]
 *
 *  Do the same also if there are intermediate inaccessible parameter accessor forwarders.
 *  The aim of this transformation is to avoid redundant parameter accessor fields.
 */
class ParamForwarding(thisTransformer: DenotTransformer) {
  import ast.tpd._

  def forwardParamAccessors(impl: Template)(implicit ctx: Context): Template = {
    def fwd(stats: List[Tree])(implicit ctx: Context): List[Tree] = {
      val (superArgs, superParamNames) = impl.parents match {
        case superCall @ Apply(fn, args) :: _ =>
          fn.tpe.widen match {
            case MethodType(paramNames, _) => (args, paramNames)
            case _ => (Nil, Nil)
          }
        case _ => (Nil, Nil)
      }
      def inheritedAccessor(sym: Symbol): Symbol = {
        val candidate = sym.owner.asClass.superClass
          .info.decl(sym.name).suchThat(_ is (ParamAccessor, butNot = Mutable)).symbol
        if (candidate.isAccessibleFrom(currentClass.thisType, superAccess = true)) candidate
        else if (candidate is Method) inheritedAccessor(candidate)
        else NoSymbol
      }
      def forwardParamAccessor(stat: Tree): Tree = {
        stat match {
          case stat: ValDef =>
            val sym = stat.symbol.asTerm
            if (sym is (PrivateLocalParamAccessor, butNot = Mutable)) {
              val idx = superArgs.indexWhere(_.symbol == sym)
              if (idx >= 0 && superParamNames(idx) == stat.name) { // supercall to like-named parameter
                val alias = inheritedAccessor(sym)
                if (alias.exists) {
                  def forwarder(implicit ctx: Context) = {
                    sym.copySymDenotation(initFlags = sym.flags | Method, info = sym.info.ensureMethodic)
                      .installAfter(thisTransformer)
                    val superAcc =
                      Super(This(currentClass), tpnme.EMPTY, inConstrCall = false).select(alias)
                    DefDef(sym, superAcc.ensureConforms(sym.info.widen))
                  }
                  return forwarder(ctx.withPhase(thisTransformer.next))
                }
              }
            }
          case _ =>
        }
        stat
      }
      stats map forwardParamAccessor
    }

    cpy.Template(impl)(body = fwd(impl.body)(ctx.withPhase(thisTransformer)))
  }
}
