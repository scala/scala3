package dottyBench.tools
package dotc
package config

import core._
import Contexts._, Symbols._, Names._, NameOps._, Phases._
import StdNames.nme
import Decorators.{given _}
import util.SourcePosition
import SourceVersion._
import reporting.Message

object Feature:

/** Is `feature` enabled by by a command-line setting? The enabling setting is
   *
   *       -language:<prefix>feature
   *
   *  where <prefix> is the fully qualified name of `owner`, followed by a ".",
   *  but subtracting the prefix `scala.language.` at the front.
   */
  def enabledBySetting(feature: TermName, owner: Symbol = NoSymbol)(using Ctx, CState): Boolean =
    def toPrefix(sym: Symbol): String =
      if !sym.exists || sym == defn.LanguageModule.moduleClass then ""
      else toPrefix(sym.owner) + sym.name.stripModuleClassSuffix + "."
    val prefix = if owner.exists then toPrefix(owner) else ""
    ctx.base.settings.language.value.contains(prefix + feature)

  /** Is `feature` enabled by by an import? This is the case if the feature
   *  is imported by a named import
   *
   *       import owner.feature
   *
   *  and there is no visible nested import that excludes the feature, as in
   *
   *       import owner.{ feature => _ }
   */
  def enabledByImport(feature: TermName, owner: Symbol = NoSymbol)(using Ctx, CState): Boolean =
    atPhase(typerPhase) {
      ctx.importInfo != null
      && ctx.importInfo.featureImported(feature,
          if owner.exists then owner else defn.LanguageModule.moduleClass)
    }

  /** Is `feature` enabled by either a command line setting or an import?
   *  @param  feature   The name of the feature
   *  @param  owner     The prefix symbol (nested in `scala.language`) where the
   *                    feature is defined.
   */
  def enabled(feature: TermName, owner: Symbol = NoSymbol)(using Ctx, CState): Boolean =
    enabledBySetting(feature, owner) || enabledByImport(feature, owner)

  /** Is auto-tupling enabled? */
  def autoTuplingEnabled(using Ctx, CState): Boolean =
    !enabled(nme.noAutoTupling)

  def dynamicsEnabled(using Ctx, CState): Boolean =
    enabled(nme.dynamics)

  def dependentEnabled(using Ctx, CState) =
    enabled(nme.dependent, defn.LanguageExperimentalModule.moduleClass)

  def scala2ExperimentalMacroEnabled(using Ctx, CState) =
    enabled("macros".toTermName, defn.LanguageExperimentalModule.moduleClass)

  def sourceVersionSetting(using Ctx, CState): SourceVersion =
    SourceVersion.valueOf(ctx.settings.source.value)

  def sourceVersion(using Ctx, CState): SourceVersion =
    if ctx.compilationUnit == null then sourceVersionSetting
    else ctx.compilationUnit.sourceVersion.getOrElse(sourceVersionSetting)

  def migrateTo3(using Ctx, CState): Boolean =
    sourceVersion == `3.0-migration` || enabledBySetting(nme.Scala2Compat)

  /** If current source migrates to `version`, issue given warning message
   *  and return `true`, otherwise return `false`.
   */
  def warnOnMigration(msg: Message, pos: SourcePosition,
      version: SourceVersion = defaultSourceVersion)(using Ctx, CState): Boolean =
    if sourceVersion.isMigrating && sourceVersion.stable == version
       || version == `3.0` && migrateTo3
    then
      report.migrationWarning(msg, pos)
      true
    else
      false

end Feature