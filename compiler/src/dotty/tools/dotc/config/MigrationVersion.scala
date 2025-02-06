package dotty.tools
package dotc
package config

import SourceVersion.*
import Feature.*
import core.Contexts.Context

enum MigrationVersion(val warnFrom: SourceVersion, val errorFrom: SourceVersion):
  case Scala2to3 extends MigrationVersion(`3.0`, `3.0`)
  case OverrideValParameter extends MigrationVersion(`3.0`, future)
  // we tighten for-comprehension without `case` to error in 3.4,
  // but we keep pat-defs as warnings for now ("@unchecked"),
  // until we propose an alternative way to assert exhaustivity to the typechecker.
  case ForComprehensionPatternWithoutCase extends MigrationVersion(`3.2`,  `3.4`)
  case ForComprehensionUncheckedPathDefs extends MigrationVersion(`3.2`,  future)

  case NonLocalReturns extends MigrationVersion(`3.2`, future)
  case AscriptionAfterPattern extends MigrationVersion(`3.3`, future)
  case ExplicitContextBoundArgument extends MigrationVersion(`3.4`, `3.5`)
  case AlphanumericInfix extends MigrationVersion(`3.4`, future)
  case RemoveThisQualifier extends MigrationVersion(`3.4`, future)
  case UninitializedVars extends MigrationVersion(`3.4`, future)
  case VarargSpliceAscription extends MigrationVersion(`3.4`, future)
  case WildcardType extends MigrationVersion(`3.4`, future)
  case WithOperator extends MigrationVersion(`3.4`, future)
  case FunctionUnderscore extends MigrationVersion(`3.4`, future)
  case NonNamedArgumentInJavaAnnotation extends MigrationVersion(`3.6`, `3.6`)
  case AmbiguousNamedTupleSyntax extends MigrationVersion(`3.6`, future)
  case ImportWildcard extends MigrationVersion(future, future)
  case ImportRename extends MigrationVersion(future, future)
  case ParameterEnclosedByParenthesis extends MigrationVersion(future, future)
  case XmlLiteral extends MigrationVersion(future, future)
  case GivenSyntax extends MigrationVersion(future, never)
  case ImplicitParamsWithoutUsing extends MigrationVersion(`3.7`, future)

  require(warnFrom.ordinal <= errorFrom.ordinal)

  def needsPatch(using Context): Boolean =
    sourceVersion.isMigrating && sourceVersion.isAtLeast(warnFrom)

  def patchFrom: SourceVersion = warnFrom.prevMigrating

end MigrationVersion
