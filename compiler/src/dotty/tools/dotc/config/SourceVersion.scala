package dotty.tools
package dotc
package config

import core.Decorators.*
import util.Property

import scala.annotation.threadUnsafe
import dotty.tools.dotc.core.Names.Name

/** Enum expressing series of source versions,
 *  where each series begins by a migration version, followed by a series of stable versions.
 *  e.g. `3.0-migration`, `3.0`, `3.1` make one series, `3.0`, as they describe the same semantics.
 *  `future-migration`, then begins the another series of stable versions, `future`, because in this version we
 *  enable more features.
 *
 *  @note This enum does not need to correspond to the scala.language imports. E.g. if a user imports
 *  `scala.language.3.1-migration`, the SourceVersion will be set to `3.1` (see `lookupSourceVersion`).
 *
 */
enum SourceVersion:
  case `3.0-migration`, `3.0`, `3.1` // Note: do not add `3.1-migration` here, 3.1 is the same language as 3.0.
  case `3.2`, `3.2-migration` // !!! DELETE `3.2-migration` BEFORE RELEASING 3.2.0 if we do not enable features from `future`
  case `future-migration`, `future`

  val isMigrating: Boolean = toString.endsWith("-migration")

  private inline def nextVersion: SourceVersion = SourceVersion.fromOrdinal(ordinal + 1)
  private inline def previousVersion: SourceVersion = SourceVersion.fromOrdinal(ordinal - 1)

  @threadUnsafe lazy val series: SourceVersion =
    if isMigrating then nextVersion else previousVersion.series

  def isAtLeast(v: SourceVersion) = this.series.ordinal >= v.series.ordinal

object SourceVersion extends Property.Key[SourceVersion]:
  def defaultSourceVersion = `3.2`

  object lookupSourceVersion:

    /** A map from with keys matching the `scala.language` imports,
     *  and values being the corresponding `SourceVersion`, or the next
     *  stable version if the key ends with `-migration` and no matching SourceVersion exists.
     */
    val fromSetting: Map[String, SourceVersion] =
      val (migratingVersions, stableVersions) = values.partition(_.isMigrating)
      val entries = stableVersions.flatMap(stable =>
        val migratingKey = s"${stable}-migration"
        val migratingEntry =
          migratingVersions.find(_.toString == migratingKey) match
            case Some(migrating) => migratingKey -> migrating
            case _               => migratingKey -> stable
        val stableEntry = stable.toString -> stable
        stableEntry :: migratingEntry :: Nil
      )
      Map.from(entries)

    /** An immutable array with keys matching the `scala.language` imports
     *  corresponding to source versions
     */
    val fromImport: IArray[Name] =
      IArray.from(fromSetting.iterator.map((key, _) => key.toTermName))

end SourceVersion
