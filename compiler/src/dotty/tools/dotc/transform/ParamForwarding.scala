package dotty.tools.dotc
package transform

import core._
import ast.Trees._
import Contexts._, Types._, Symbols._, Flags._, TypeUtils._, DenotTransformers._, StdNames._
import Decorators._, NameOps._
import config.Printers.typr

/** For all parameter accessors
 *
 *      val x: T = ...
 *
 *  if
 *  (1) x is forwarded in the supercall to a parameter that's also named `x`
 *  (2) the superclass parameter accessor for `x` is accessible from the current class
 *  change the accessor to
 *
 *      def x: T = super.x.asInstanceOf[T]
 *
 *  Do the same also if there are intermediate inaccessible parameter accessor forwarders.
 *  The aim of this transformation is to avoid redundant parameter accessor fields.
 */
class ParamForwarding(thisPhase: DenotTransformer) {
  import ast.tpd._

  def forwardParamAccessors(impl: Template)(implicit ctx: Context): Template = {
    def fwd(stats: List[Tree])(implicit ctx: Context): List[Tree] = {
      val (superArgs, superParamNames) = impl.parents match {
        case superCall @ Apply(fn, args) :: _ =>
          fn.tpe.widen match {
            case MethodType(paramNames) => (args, paramNames)
            case _ => (Nil, Nil)
          }
        case _ => (Nil, Nil)
      }
      def inheritedAccessor(sym: Symbol): Symbol = {
        /**
         * Dmitry: having it have the same name is needed to maintain correctness in presence of subclassing
         * if you would use parent param-name `a` to implement param-field `b`
         * overriding field `b` will actually override field `a`, that is wrong!
         *
         * class A(val s: Int);
         * class B(val b: Int) extends A(b)
         * class C extends A(2) {
         *   def s = 3
         *   assert(this.b == 2)
         * }
         */
        val candidate = sym.owner.asClass.superClass
          .info.decl(sym.name).suchThat(_ is (ParamAccessor, butNot = Mutable)).symbol
        if (candidate.isAccessibleFrom(currentClass.thisType, superAccess = true)) candidate
        else if (candidate.exists) inheritedAccessor(candidate)
        else NoSymbol
      }
      def forwardParamAccessor(stat: Tree): Tree = {
        stat match {
          case stat: ValDef =>
            val sym = stat.symbol.asTerm
            if (sym.is(ParamAccessor, butNot = Mutable) && !sym.info.isInstanceOf[ExprType]) {
              // ElimByName gets confused with methods returning an ExprType,
              // so avoid param forwarding if parameter is by name. See i1766.scala
              val idx = superArgs.indexWhere(_.symbol == sym)
              if (idx >= 0 && superParamNames(idx) == stat.name) { // supercall to like-named parameter
                val alias = inheritedAccessor(sym)
                if (alias.exists) {
                  def forwarder(implicit ctx: Context) = {
                    sym.denot.copySymDenotation(initFlags = sym.flags | Method | Stable, info = sym.info.ensureMethodic)
                      .installAfter(thisPhase)
                    val superAcc =
                      Super(This(currentClass), tpnme.EMPTY, inConstrCall = false).select(alias)
                    typr.println(i"adding param forwarder $superAcc")
                    DefDef(sym, superAcc.ensureConforms(sym.info.widen)).withPos(stat.pos)
                  }
                  return forwarder(ctx.withPhase(thisPhase.next))
                }
              }
            }
          case _ =>
        }
        stat
      }
      stats map forwardParamAccessor
    }

    cpy.Template(impl)(body = fwd(impl.body)(ctx.withPhase(thisPhase)))
  }
}
