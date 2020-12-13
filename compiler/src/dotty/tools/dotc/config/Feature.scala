package dotty.tools
package dotc
package config

import core._
import Contexts._, Symbols._, Names._, NameOps._, Phases._
import StdNames.nme
import Decorators.{_, given}
import util.SrcPos
import SourceVersion._
import reporting.Message

object Feature:

  private val dependent = "dependent".toTermName
  private val namedTypeArguments = "namedTypeArguments".toTermName
  private val genericNumberLiterals = "genericNumberLiterals".toTermName

/** Is `feature` enabled by by a command-line setting? The enabling setting is
   *
   *       -language:<prefix>feature
   *
   *  where <prefix> is the fully qualified name of `owner`, followed by a ".",
   *  but subtracting the prefix `scala.language.` at the front.
   */
  def enabledBySetting(feature: TermName, owner: Symbol = NoSymbol)(using Context): Boolean =
    def toPrefix(sym: Symbol): String =
      if !sym.exists || sym == defn.LanguageModule.moduleClass then ""
      else toPrefix(sym.owner) + sym.name.stripModuleClassSuffix + "."
    val prefix = if owner ne NoSymbol then toPrefix(owner) else ""
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
  def enabledByImport(feature: TermName, owner: Symbol = NoSymbol)(using Context): Boolean =
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
  def enabled(feature: TermName, owner: Symbol = NoSymbol)(using Context): Boolean =
    enabledBySetting(feature, owner) || enabledByImport(feature, owner)

  /** Is auto-tupling enabled? */
  def autoTuplingEnabled(using Context): Boolean =
    !enabled(nme.noAutoTupling)

  def dynamicsEnabled(using Context): Boolean =
    enabled(nme.dynamics)

  def dependentEnabled(using Context) =
    enabled(dependent, defn.LanguageExperimentalModule.moduleClass)

  def namedTypeArgsEnabled(using Context) =
    enabled(namedTypeArguments, defn.LanguageExperimentalModule.moduleClass)

  def genericNumberLiteralsEnabled(using Context) =
    enabled(genericNumberLiterals, defn.LanguageExperimentalModule.moduleClass)

  def scala2ExperimentalMacroEnabled(using Context) =
    enabled("macros".toTermName, defn.LanguageExperimentalModule.moduleClass)

  def sourceVersionSetting(using Context): SourceVersion =
    SourceVersion.valueOf(ctx.settings.source.value)

  def sourceVersion(using Context): SourceVersion =
    if ctx.compilationUnit == null then sourceVersionSetting
    else ctx.compilationUnit.sourceVersion match
      case Some(v) => v
      case none => sourceVersionSetting

  def migrateTo3(using Context): Boolean = sourceVersion == `3.0-migration`

  /** If current source migrates to `version`, issue given warning message
   *  and return `true`, otherwise return `false`.
   */
  def warnOnMigration(msg: Message, pos: SrcPos,
      version: SourceVersion = defaultSourceVersion)(using Context): Boolean =
    if sourceVersion.isMigrating && sourceVersion.stable == version
       || version == `3.0` && migrateTo3
    then
      report.migrationWarning(msg, pos)
      true
    else
      false

end Feature