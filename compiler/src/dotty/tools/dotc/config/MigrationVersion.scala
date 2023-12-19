package dotty.tools
package dotc
package config

import SourceVersion.*
import Feature.*
import core.Contexts.Context

class MigrationVersion(val warnFrom: SourceVersion, val errorFrom: SourceVersion):
  assert(warnFrom.ordinal <= errorFrom.ordinal)
  def needsPatch(using Context): Boolean =
    sourceVersion.isMigrating && sourceVersion.isAtLeast(errorFrom)

object MigrationVersion:

  val Scala2to3 = MigrationVersion(`3.0`, `3.0`)

  val OverrideValParameter = MigrationVersion(`3.0`, future)

  // we tighten for-comprehension without `case` to error in 3.4,
  // but we keep pat-defs as warnings for now ("@unchecked"),
  // until we propose an alternative way to assert exhaustivity to the typechecker.
  val ForComprehensionPatternWithoutCase = MigrationVersion(`3.2`,  `3.4`)
  val ForComprehensionUncheckedPathDefs = MigrationVersion(`3.2`,  future)

  val NonLocalReturns = MigrationVersion(`3.2`, future)

  val AscriptionAfterPattern = MigrationVersion(`3.3`, future)

  val AlphanumericInfix = MigrationVersion(`3.4`, future)
  val RemoveThisQualifier = MigrationVersion(`3.4`, future)
  val UninitializedVars = MigrationVersion(`3.4`, future)
  val VarargSpliceAscription = MigrationVersion(`3.4`, future)
  val WildcardType = MigrationVersion(`3.4`, future)
  val WithOperator = MigrationVersion(`3.4`, future)
  val FunctionUnderscore = MigrationVersion(`3.4`, future)

  val ImportWildcard = MigrationVersion(future, future)
  val ImportRename = MigrationVersion(future, future)
  val ParameterEnclosedByParenthesis = MigrationVersion(future, future)
  val XmlLiteral = MigrationVersion(future, future)

end MigrationVersion
