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

  val allSourceVersionNames = values.toList.map(_.toString.toTermName)
end SourceVersion
