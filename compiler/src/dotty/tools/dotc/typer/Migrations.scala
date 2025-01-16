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
import config.MigrationVersion as mv
import config.Feature.sourceVersion
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

  /** Run `migration`, asserting we are in the proper Typer (not a ReTyper) */
  inline def migrate[T](inline migration: T): T =
    assert(!this.isInstanceOf[ReTyper])
    migration

  /** Run `migration`, provided we are in the proper Typer (not a ReTyper) */
  inline def migrate(inline migration: Unit): Unit =
    if !this.isInstanceOf[ReTyper] then migration

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
        em"`?` is not a valid type name$addendum", namePos, mv.Scala2to3)

  def typedAsFunction(tree: untpd.PostfixOp, pt: Type)(using Context): Tree = {
    val untpd.PostfixOp(qual, Ident(nme.WILDCARD)) = tree: @unchecked
    val pt1 = if (defn.isFunctionNType(pt)) pt else AnyFunctionProto
    val nestedCtx = ctx.fresh.setNewTyperState()
    val res = typed(qual, pt1)(using nestedCtx)
    res match {
      case blockEndingInClosure(_, _, _) =>
      case _ =>
        val recovered = typed(qual)(using ctx.fresh.setExploreTyperState())
        val msg = OnlyFunctionsCanBeFollowedByUnderscore(recovered.tpe.widen, tree)
        report.errorOrMigrationWarning(msg, tree.srcPos, mv.Scala2to3)
        if mv.Scala2to3.needsPatch then
          // Under -rewrite, patch `x _` to `(() => x)`
          msg.actions
            .headOption
            .foreach(Rewrites.applyAction)
          return typed(untpd.Function(Nil, qual), pt)
    }
    nestedCtx.typerState.commit()

    def functionPrefixSuffix(arity: Int) = if (arity > 0) ("", "") else ("(() => ", "())")

    lazy val (prefix, suffix) = res match {
      case Block(DefDef(_, vparams :: Nil, _, _) :: Nil, _: Closure) =>
        functionPrefixSuffix(vparams.length)
      case Block(ValDef(_, _, _) :: Nil, Block(DefDef(_, vparams :: Nil, _, _) :: Nil, _: Closure)) =>
        functionPrefixSuffix(vparams.length)
      case _ =>
        ("(() => ", ")")
    }
    val mversion = mv.FunctionUnderscore
    def remedy =
      if ((prefix ++ suffix).isEmpty) "simply leave out the trailing ` _`"
      else s"use `$prefix<function>$suffix` instead"
    def rewrite = Message.rewriteNotice("This construct", mversion.patchFrom)
    report.errorOrMigrationWarning(
      em"""The syntax `<function> _` is no longer supported;
          |you can $remedy$rewrite""",
      tree.srcPos, mversion)
    if mversion.needsPatch then
      patch(Span(tree.span.start), prefix)
      patch(Span(qual.span.end, tree.span.end), suffix)

    res
  }

  /** Flag & migrate explicit normal arguments to parameters coming from context bounds
   *  Warning in 3.4, error in 3.5, rewrite in 3.5-migration.
   */
  def contextBoundParams(tree: Tree, tp: Type, pt: FunProto)(using Context): Unit =
    val mversion = mv.ExplicitContextBoundArgument
    def isContextBoundParams = tp.stripPoly match
      case MethodType(ContextBoundParamName(_) :: _) => true
      case _ => false
    if sourceVersion.isAtLeast(`3.4`)
      && isContextBoundParams
      && pt.applyKind != ApplyKind.Using
    then
      def rewriteMsg =
        if pt.args.isEmpty then ""
        else Message.rewriteNotice("This code", mversion.patchFrom)
      report.errorOrMigrationWarning(
        em"""Context bounds will map to context parameters.
            |A `using` clause is needed to pass explicit arguments to them.$rewriteMsg""",
        tree.srcPos, mversion)
      tree match
        case Apply(ta @ TypeApply(Select(New(_), _), _), Nil) =>
          // Remove empty arguments for calls to new that may precede the context bound.
          // They are no longer necessary.
          patch(Span(ta.span.end, pt.args.head.span.start - 1), "")
        case _ => ()
      if mversion.needsPatch && pt.args.nonEmpty then
        patch(Span(pt.args.head.span.start), "using ")
  end contextBoundParams

end Migrations
