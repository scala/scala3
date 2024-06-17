package dotty.tools
package dotc
package config

import core.Decorators.*
import util.Property

enum SourceVersion:
  case `3.0-migration`, `3.0`, `3.1` // Note: do not add `3.1-migration` here, 3.1 is the same language as 3.0.
  case `3.2-migration`, `3.2`
  case `3.3-migration`, `3.3`
  case `3.4-migration`, `3.4`
  case `3.5-migration`, `3.5`
  case `3.6-migration`, `3.6`
  // !!! Keep in sync with scala.runtime.stdlibPatches.language !!!
  case `future-migration`, `future`

  val isMigrating: Boolean = toString.endsWith("-migration")

  def stable: SourceVersion =
    if isMigrating then SourceVersion.values(ordinal + 1) else this

  def prevMigrating: SourceVersion =
    if isMigrating then this else SourceVersion.values(ordinal - 1).prevMigrating

  def isAtLeast(v: SourceVersion) = stable.ordinal >= v.ordinal

  def isAtMost(v: SourceVersion) = stable.ordinal <= v.ordinal

object SourceVersion extends Property.Key[SourceVersion]:
  def defaultSourceVersion = `3.5`

  /** language versions that may appear in a language import, are deprecated, but not removed from the standard library. */
  val illegalSourceVersionNames = List("3.1-migration").map(_.toTermName)

  /** language versions that the compiler recognises. */
  val validSourceVersionNames = values.toList.map(_.toString.toTermName)

  /** All source versions that can be recognised from a language import. e.g. `import language.3.1` */
  val allSourceVersionNames = validSourceVersionNames ::: illegalSourceVersionNames
end SourceVersion
