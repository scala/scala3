package dotty.tools.dotc
package cc

import core.*
import Contexts.*, Symbols.defn, Types.*, Names.*
import config.Feature

/** Preserves user-written closure parameter names through type rebuilds, so
 *  that capture checking can recover them when expanding plain `FunctionN`
 *  inferred types into dependent function types.
 *
 *  Lifecycle:
 *
 *   - [[record]] is called when a `Closure` is assigned its inferred type.
 *     If that type is a plain (non-refined) `FunctionN` with user-given param
 *     names, the names are stored against the `AppliedType`.
 *
 *   - [[propagate]] is called from [[Types.AppliedType.derivedAppliedType]]
 *     each time an `AppliedType` is rebuilt (e.g. by PostTyper inserting
 *     `@caps.declared` annotations, or by capture-set substitution). It
 *     transfers the registration to the rebuilt type so identity lookups
 *     keep working.
 *
 *   - [[of]] is read by `cc.Setup.normalizeFunctions` when expanding a plain
 *     `FunctionN` to a dependent function: if names are registered, they
 *     replace the synthetic `x$N` parameter names that would otherwise be used.
 *
 *  The backing table is a per-`Run` `EqHashMap` (see `cc.CaptureRunInfo`).
 *  Off the CC code path the table stays empty, so [[propagate]] is essentially
 *  free.
 */
object ClosureParamNames:

  /** Register the user-written term-parameter names of a closure against the
   *  resulting `funType`. No-op unless `funType` is a non-refined `FunctionN`
   *  applied type with at least one named parameter.
   */
  def record(funType: Type, methodicType: Type)(using Context): Unit =
    if !Feature.ccEnabledSomewhere then return
    val run = ctx.run
    if run == null then return
    (funType, methodicType) match
      case (applied: AppliedType, mt: MethodType)
      if defn.isFunctionNType(applied) && mt.paramNames.nonEmpty =>
        run.closureParamNames(applied) = mt.paramNames
      case _ =>

  /** Carry over any registration from `original` to `rebuilt`. Called from
   *  every `AppliedType.derivedAppliedType` rebuild, so it returns immediately
   *  when capture checking is not in play.
   */
  def propagate(original: AppliedType, rebuilt: Type)(using Context): Unit =
    if !Feature.ccEnabledSomewhere then return
    val run = ctx.run
    if run == null then return
    rebuilt match
      case applied: AppliedType =>
        val names = run.closureParamNames.lookup(original)
        if names != null then run.closureParamNames(applied) = names
      case _ =>

  /** User-written parameter names registered for `tp`, or `Nil` if none. */
  def of(tp: Type)(using Context): List[TermName] =
    val run = ctx.run
    if run == null then Nil
    else
      val names = run.closureParamNames.lookup(tp)
      if names == null then Nil else names

end ClosureParamNames
