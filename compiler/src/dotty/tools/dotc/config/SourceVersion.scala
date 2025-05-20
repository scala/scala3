package dotty.tools
package dotc
package config

import core.Decorators.*
import util.Property

enum SourceVersion:
  case `3.0-migration`, `3.0`
  case `3.1-migration`, `3.1`
  case `3.2-migration`, `3.2`
  case `3.3-migration`, `3.3`
  case `future-migration`, `future`
  case `never`    // needed for MigrationVersion.errorFrom if we never want to issue an error

  val isMigrating: Boolean = toString.endsWith("-migration")

  def stable: SourceVersion =
    if isMigrating then SourceVersion.values(ordinal + 1) else this

  def isAtLeast(v: SourceVersion) = stable.ordinal >= v.ordinal

  def isAtMost(v: SourceVersion) = stable.ordinal <= v.ordinal

object SourceVersion extends Property.Key[SourceVersion]:

  def defaultSourceVersion = `3.3`

  /* Illegal source versions that may not appear in the settings `-source:<...>` */
  val illegalInSettings = List(`3.1-migration`, `never`)

  /* Illegal source versions that may not appear as an import `import scala.language.<...>` */
  val illegalInImports  = List(`3.1-migration`, `never`)

  /** language versions that may appear in a language import, are deprecated, but not removed from the standard library. */
  val illegalSourceVersionNames = illegalInImports.map(_.toString.toTermName)

  /** language versions that the compiler recognises. */
  val validSourceVersionNames = values.toList.map(_.toString.toTermName)

  /** All source versions that can be recognised from a language import. e.g. `import language.3.1` */
  val allSourceVersionNames = validSourceVersionNames ::: illegalSourceVersionNames
end SourceVersion
