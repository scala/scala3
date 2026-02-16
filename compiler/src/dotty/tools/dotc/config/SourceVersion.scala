package dotty.tools
package dotc
package config

import core.Decorators.*
import core.Contexts.*
import Feature.isPreviewEnabled
import util.Property

enum SourceVersion:

  case `3.0-migration`, `3.0`
  case `3.1-migration`, `3.1`
  case `3.2-migration`, `3.2`
  case `3.3-migration`, `3.3`
  case `3.4-migration`, `3.4`
  case `3.5-migration`, `3.5`
  case `3.6-migration`, `3.6`
  case `3.7-migration`, `3.7`
  case `3.8-migration`, `3.8`
  case `3.9-migration`, `3.9`
  // Add 3.x-migration and 3.x here
  // !!! Keep in sync with scala.runtime.stdlibPatches.language !!!
  case `2.13`
  case `future-migration`, `future`

  case `never`    // needed for MigrationVersion.errorFrom if we never want to issue an error

  val isMigrating: Boolean = toString.endsWith("-migration")

  def stable: SourceVersion =
    if isMigrating then SourceVersion.values(ordinal + 1) else this

  def prevMigrating: SourceVersion =
    if isMigrating then this else SourceVersion.values(ordinal - 1).prevMigrating

  def isAtLeast(v: SourceVersion) = stable.ordinal >= v.ordinal

  def isAtMost(v: SourceVersion) = stable.ordinal <= v.ordinal

  def isScala2 = this == `2.13`

  def enablesFewerBraces = isAtLeast(`3.3`)
  def enablesClauseInterleaving = isAtLeast(`3.6`)
  def enablesNewGivens = isAtLeast(`3.6`)
  def enablesNamedTuples = isAtLeast(`3.7`)
  def enablesBetterFors(using Context) = isAtLeast(`3.8`) || (isAtLeast(`3.7`) && isPreviewEnabled)
  /** See PR #23441 and tests/neg/i23435-min */
  def enablesDistributeAnd = !isAtLeast(`future`)
  def enablesCompactAnnotation = isAtLeast(`3.9`)

  def requiresNewSyntax = isAtLeast(future)

object SourceVersion extends Property.Key[SourceVersion]:

  /* The default source version used by the built compiler */
  val defaultSourceVersion = `3.8`

  /* Illegal source versions that may not appear in the settings `-source:<...>` */
  val illegalInSettings = List(`2.13`, `3.1-migration`, `never`)

  /* Illegal source versions that may not appear as an import `import scala.language.<...>` */
  val illegalInImports  = List(`3.1-migration`, `never`)

  /** language versions that may appear in a language import, are deprecated, but not removed from the standard library. */
  val illegalSourceVersionNames = illegalInImports.map(_.toString.toTermName)

  /** language versions that the compiler recognises. */
  val validSourceVersionNames = values.toList.map(_.toString.toTermName)

  /** All source versions that can be recognised from a language import. e.g. `import language.3.1` */
  val allSourceVersionNames = validSourceVersionNames ::: illegalSourceVersionNames
end SourceVersion
