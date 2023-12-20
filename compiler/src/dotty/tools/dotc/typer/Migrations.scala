package dotty.tools
package dotc
package typer

import core.*
import ast.*
import Contexts.*
import Types.*
import Flags.*
import Names.*
import StdNames.*
import Symbols.*
import Trees.*
import ProtoTypes.*
import Decorators.*
import config.MigrationVersion
import config.Feature.{sourceVersion, migrateTo3}
import config.SourceVersion.*
import reporting.*
import NameKinds.ContextBoundParamName
import rewrites.Rewrites.patch
import util.Spans.Span

/** A utility module containing source-dependent deprecation messages
 *  and migrations
 */
object Migrations:

  import tpd.*

  /** Flag & migrate `?` used as a higher-kinded type parameter
   *  Warning in 3.0-migration, error from 3.0
   */
  def migrateKindProjectorQMark(tree: untpd.TypeDef, sym: Symbol)(using Context): Unit =
    if tree.name eq tpnme.? then
      val addendum = if sym.owner.is(TypeParam)
        then ", use `_` to denote a higher-kinded type parameter"
        else ""
      val namePos = tree.sourcePos.withSpan(tree.nameSpan)
      report.errorOrMigrationWarning(
        em"`?` is not a valid type name$addendum", namePos, MigrationVersion.Scala2to3)

  /** Flag & migrate explicit normal arguments to parameters coming from context bounds
   *  Warning in 3.4, error in 3.5, rewrite in 3.5-migration.
   */
  def migrateContextBoundParams(tree: Tree, tp: Type, pt: FunProto)(using Context): Unit =
    def isContextBoundParams = tp.stripPoly match
      case MethodType(ContextBoundParamName(_) :: _) => true
      case _ => false
    if sourceVersion.isAtLeast(`3.4`)
      && isContextBoundParams
      && pt.applyKind != ApplyKind.Using
    then
      def rewriteMsg = Message.rewriteNotice("This code", `3.5-migration`)
      report.errorOrMigrationWarning(
        em"""Context bounds will map to context parameters.
            |A `using` clause is needed to pass explicit arguments to them.$rewriteMsg""",
        tree.srcPos, MigrationVersion(`3.4`, `3.5`))
      if sourceVersion.isAtLeast(`3.5-migration`) then
        patch(Span(pt.args.head.span.start), "using ")
  end migrateContextBoundParams
