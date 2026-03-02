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
import config.Feature.{sourceVersion, migrateTo3}
import config.SourceVersion.*
import reporting.*
import NameKinds.ContextBoundParamName
import rewrites.Rewrites.patch
import util.Spans.Span
import rewrites.Rewrites
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourcePosition

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
      em"""The trailing ` _` for eta-expansion is unnecessary;
        |the compiler performs eta-expansion automatically, so you can write the function value directly instead.$rewrite""",
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

  /** Report implicit parameter lists and rewrite implicit parameter list to contextual params */
  def implicitParams(tree: Tree, tp: MethodOrPoly, pt: FunProto)(using Context): Unit =
    val mversion = mv.ImplicitParamsWithoutUsing
    if tp.companion == ImplicitMethodType && pt.applyKind != ApplyKind.Using && pt.args.nonEmpty && pt.args.head.span.exists then
      // The application can only be rewritten if it uses parentheses syntax.
      // See issue #22927 and related tests.
      val hasParentheses = checkParentheses(tree, pt)
      val rewriteMsg =
        if hasParentheses then
          Message.rewriteNotice("This code", mversion.patchFrom)
        else ""
      val message =
        em"""Implicit parameters should be provided with a `using` clause.$rewriteMsg
            |To disable the warning, please use the following option:
            |  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s"
            |"""
      val codeAction = CodeAction(
        title = "Add `using` clause",
        description = None,
        patches = List(ActionPatch(pt.args.head.startPos.sourcePos, "using "))
      )
      val withActions = message.withActions(codeAction)
      report.errorOrMigrationWarning(
        withActions,
        pt.args.head.srcPos,
        mversion
      )
      if hasParentheses && mversion.needsPatch then
        patchImplicitParams(tree, pt)
  end implicitParams

  private def checkParentheses(tree: Tree, pt: FunProto)(using Context): Boolean =
    val ptSpan = pt.args.head.span
    ptSpan.exists
    && tree.span.exists
    && ctx.source.content
      .slice(tree.span.end, ptSpan.start)
      .exists(_ == '(')

  private def patchImplicitParams(tree: Tree, pt: FunProto)(using Context): Unit =
    patch(Span(pt.args.head.span.start), "using ")

  object ImplicitToGiven:
    def valDef(vdef: ValDef)(using Context): Unit =
      if ctx.settings.YimplicitToGiven.value
        && vdef.symbol.is(Implicit)
        && !vdef.symbol.isParamOrAccessor
      then
        val implicitSpan =
          vdef.mods.mods.collectFirst {
            case mod: untpd.Mod.Implicit => mod.span
          }.get
        patch(
          Span(implicitSpan.start, implicitSpan.end + 1),
          ""
        )
        patch(
          Span(vdef.mods.mods.last.span.end + 1, vdef.namePos.span.start), "given "
        )

    def defDef(ddef: DefDef)(using Context): Unit =
      if
        ctx.settings.YimplicitToGiven.value
        && ddef.symbol.is(Implicit)
        && !ddef.symbol.isParamOrAccessor
        && !ddef.symbol.isOldStyleImplicitConversion()
      then
        val implicitSpan =
          ddef.mods.mods.collectFirst {
            case mod: untpd.Mod.Implicit => mod.span
          }.get
        patch(
          Span(implicitSpan.start, implicitSpan.end + 1), ""
        )
        patch(
          Span(ddef.mods.mods.last.span.end + 1, ddef.namePos.span.start), "given "
        )
        // remove empty parentheses
        patch(
          Span(ddef.namePos.span.end, ddef.tpt.span.start), ": "
        )
        ddef.tpt match
          case refinedType: untpd.RefinedTypeTree =>
            patch(refinedType.span.startPos, "(")
            patch(refinedType.span.endPos, ")")
          case _ =>

    def implicitParams(tree: Tree, tp: MethodOrPoly, pt: FunProto)(using Context): Unit =
      if
        ctx.settings.YimplicitToGiven.value
        && !mv.ExplicitContextBoundArgument.needsPatch // let's not needlessly repeat the patch
        && tp.companion == ImplicitMethodType
        && pt.applyKind != ApplyKind.Using
        && pt.args.nonEmpty
        && checkParentheses(tree, pt)
      then
          patchImplicitParams(tree, pt)


end Migrations
