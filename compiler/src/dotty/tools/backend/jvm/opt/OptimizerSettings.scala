package dotty.tools.backend.jvm.opt

import dotty.tools.dotc.core.Contexts.Context

import scala.annotation.constructorOnly

/**
 * Encapsulates settings so that the optimizer can use them without directly depending on a Context,
 * since the context outside of settings is single-threaded,
 * and it would become tempting to use it for something else.
 */
class OptimizerSettings(using @constructorOnly ctx: Context):
  val optUnreachableCode: Boolean = ctx.settings.optUnreachableCode
  val optNullnessTracking: Boolean = ctx.settings.optNullnessTracking
  val optBoxUnbox: Boolean = ctx.settings.optBoxUnbox
  val optCopyPropagation: Boolean = ctx.settings.optCopyPropagation
  val optRedundantCasts: Boolean = ctx.settings.optRedundantCasts
  val optSimplifyJumps: Boolean = ctx.settings.optSimplifyJumps
  val optCompactLocals: Boolean = ctx.settings.optCompactLocals
  val optClosureInvocations: Boolean = ctx.settings.optClosureInvocations
  val optAllowSkipCoreModuleInit: Boolean = ctx.settings.optAllowSkipCoreModuleInit
  val optAssumeModulesNonNull: Boolean = ctx.settings.optAssumeModulesNonNull
  val optAllowSkipClassLoading: Boolean = ctx.settings.optAllowSkipClassLoading
  val optInlinerEnabled: Boolean = ctx.settings.optInline.value.nonEmpty
  val optInlineFrom: List[String] = ctx.settings.optInline.value
  val optInlineHeuristics: String = ctx.settings.YoptInlineHeuristics.value
  val optWarningNoInlineMixed: Boolean = ctx.settings.optWarningNoInlineMixed
  val optWarningNoInlineMissingBytecode: Boolean = ctx.settings.optWarningNoInlineMissingBytecode
  val optWarningNoInlineMissingScalaInlineInfoAttr: Boolean = ctx.settings.optWarningNoInlineMissingScalaInlineInfoAttr
  val optWarningEmitAtInlineFailed: Boolean = ctx.settings.optWarningEmitAtInlineFailed
  val optWarningEmitAnyInlineFailed: Boolean = ctx.settings.optWarningEmitAnyInlineFailed
  val optLogInline: Option[String] = ctx.settings.YoptLogInline.valueSetByUser
  val optTrace: Option[String] = ctx.settings.YoptTrace.valueSetByUser

