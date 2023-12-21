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
import rewrites.Rewrites

/** A utility trait containing source-dependent deprecation messages
 *  and migrations.
 */
trait Migrations:
  this: Typer =>

  import tpd.*

  /** Flag & migrate `?` used as a higher-kinded type parameter
   *  Warning in 3.0-migration, error from 3.0
   */
  def kindProjectorQMark(tree: untpd.TypeDef, sym: Symbol)(using Context): Unit =
    if tree.name eq tpnme.? then
      val addendum = if sym.owner.is(TypeParam)
        then ", use `_` to denote a higher-kinded type parameter"
        else ""
      val namePos = tree.sourcePos.withSpan(tree.nameSpan)
      report.errorOrMigrationWarning(
        em"`?` is not a valid type name$addendum", namePos, MigrationVersion.Scala2to3)

  def typedAsFunction(tree: untpd.PostfixOp, pt: Type)(using Context): Tree = {
    val untpd.PostfixOp(qual, Ident(nme.WILDCARD)) = tree: @unchecked
    val pt1 = if (defn.isFunctionNType(pt)) pt else AnyFunctionProto
    val nestedCtx = ctx.fresh.setNewTyperState()
    val res = typed(qual, pt1)(using nestedCtx)
    res match {
      case closure(_, _, _) =>
      case _ =>
        val recovered = typed(qual)(using ctx.fresh.setExploreTyperState())
        val msg = OnlyFunctionsCanBeFollowedByUnderscore(recovered.tpe.widen, tree)
        report.errorOrMigrationWarning(msg, tree.srcPos, MigrationVersion.Scala2to3)
        if MigrationVersion.Scala2to3.needsPatch then
          // Under -rewrite, patch `x _` to `(() => x)`
          msg.actions
            .headOption
            .foreach(Rewrites.applyAction)
          return typed(untpd.Function(Nil, qual), pt)
    }
    nestedCtx.typerState.commit()

    lazy val (prefix, suffix) = res match {
      case Block(mdef @ DefDef(_, vparams :: Nil, _, _) :: Nil, _: Closure) =>
        val arity = vparams.length
        if (arity > 0) ("", "") else ("(() => ", "())")
      case _ =>
        ("(() => ", ")")
    }
    def remedy =
      if ((prefix ++ suffix).isEmpty) "simply leave out the trailing ` _`"
      else s"use `$prefix<function>$suffix` instead"
    def rewrite = Message.rewriteNotice("This construct", `3.4-migration`)
    report.errorOrMigrationWarning(
      em"""The syntax `<function> _` is no longer supported;
          |you can $remedy$rewrite""",
      tree.srcPos,
      MigrationVersion.FunctionUnderscore)
    if MigrationVersion.FunctionUnderscore.needsPatch then
      patch(Span(tree.span.start), prefix)
      patch(Span(qual.span.end, tree.span.end), suffix)

    res
  }

  /** Flag & migrate explicit normal arguments to parameters coming from context bounds
   *  Warning in 3.4, error in 3.5, rewrite in 3.5-migration.
   */
  def contextBoundParams(tree: Tree, tp: Type, pt: FunProto)(using Context): Unit =
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
  end contextBoundParams

end Migrations
