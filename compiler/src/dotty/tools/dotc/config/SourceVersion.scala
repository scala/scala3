package dotty.tools
package dotc
package config

import core.Decorators.*
import util.Property

enum SourceVersion:
  case `3.0-migration`, `3.0`, `3.1`, `3.2`, `future-migration`, `future`

  val isMigrating: Boolean = toString.endsWith("-migration")

  def stable: SourceVersion =
    if isMigrating then SourceVersion.values(ordinal + 1) else this

  def isAtLeast(v: SourceVersion) = stable.ordinal >= v.ordinal

object SourceVersion extends Property.Key[SourceVersion]:

  /** language versions that may appear in a language import, are deprecated, but not removed from the standard library. */
  val illegalSourceVersionNames = List("3.1-migration").map(_.toTermName)

  /** language versions that the compiler recognises. */
  val validSourceVersionNames = values.toList.map(_.toString.toTermName)

  /** All source versions that can be recognised from a language import. e.g. `import language.3.1` */
  val allSourceVersionNames = validSourceVersionNames ::: illegalSourceVersionNames
end SourceVersion
