package dotty.tools
package dotc
package typer

import core._
import Contexts._, Types._, Flags._, Denotations._, NameOps._, Symbols._
import annotation.unchecked

object Inferencing {

  implicit class Infer(val ictx: Context) extends AnyVal {

    implicit private def ctx = ictx
    private def state = ctx.typerState

    /** Add all parameters in given polytype `pt` to the constraint's domain.
     *  If the constraint contains already some of these parameters in its domain,
     *  make a copy of the polytype and add the copy's type parameters instead.
     *  Return either the original polytype, or the copy, if one was made.
     */
    def track(pt: PolyType): PolyType = {
      val tracked =
        if (state.constraint contains pt) pt.copy(pt.paramNames, pt.paramBounds, pt.resultType)
        else pt
      state.constraint = state.constraint + tracked
      tracked
    }

    /** Interpolate undetermined variables.
     *  If a variable appears covariantly in type `tp`, approximate it by
     *  its lower bound. Otherwise, if it appears contravariantly in type `tp`,
     *  approximate it by its upper bound. Otherwise, if `always` is true,
     *  approximate it also by its lower bound.
     *  Instantiated variables are removed from `undetVars`.
     */
    def interpolateUndetVars(upTo: List[TypeVar], tp: Type, always: Boolean = false): Unit = {
      def recur(undets: List[TypeVar]): List[TypeVar] =
        if (undets eq upTo) undets
        else (undets: @unchecked) match {
          case tvar :: rest =>
            def instantiate(fromBelow: Boolean) = {
              tvar.instantiateWith(ctx.typeComparer.approximate(tvar.origin, fromBelow))
              recur(rest)
            }
            val v = tp varianceOf tvar
            if (v is Covariant) instantiate(fromBelow = true)
            else if (v is Contravariant) instantiate(fromBelow = false)
            else if (always) instantiate(fromBelow = true)
            else tvar :: recur(rest)
        }
      state.undetVars = recur(state.undetVars)
    }

    def newTypeVars(pt: PolyType): List[TypeVar] = {
      val tvars =
        for (n <-  (0 until pt.paramNames.length).toList)
        yield TypeVar(PolyParam(pt, n))
      state.undetVars = tvars ++ state.undetVars
      tvars
    }

    def isSubTypes(actuals: List[Type], formals: List[Type])(implicit ctx: Context): Boolean = formals match {
      case formal :: formals1 =>
        actuals match {
          case actual :: actuals1 => actual <:< formal && isSubTypes(actuals1, formals1)
          case _ => false
        }
      case nil =>
        actuals.isEmpty
    }
/* not needed right now
    def formalParameters[T](mtp: MethodType, actuals: List[T])(isRepeated: T => Boolean)(implicit ctx: Context) =
      if (mtp.isVarArgs && !(actuals.nonEmpty && isRepeated(actuals.last))) {
        val leading = mtp.paramTypes.init
        val repeated = mtp.paramTypes.last.typeArgs.head
        val trailing = List.fill(actuals.length - leading.length)(repeated)
        leading ++ trailing
      }
      else mtp.paramTypes
  */
  }
}