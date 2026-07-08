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
        if !defn.isFunctionType(recovered.tpe.widen) then
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
      case _ if defn.isFunctionType(res.tpe.widen) =>
        ("", "")
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

  /** Under `-Yapp-to-main`, rewrite `object Foo extends App { body }` into an object
   *  with an explicit `main` method, dropping the deprecated `scala.App` parent
   *  (see issue #559).
   *
   *  The rewrite preserves a leading run of member definitions as object members and
   *  wraps the trailing executable statements into `main`, keeping the brace vs.
   *  braceless style of the source. This is safe because such members keep their
   *  visibility and their initialization order is unchanged (object initialization
   *  still runs them, in source order, before `main`).
   *
   *  Anything that cannot be rewritten safely is left untouched and reported with a
   *  diagnostic pointing at `@main`, so the user knows why it was skipped. This
   *  covers definitions that only inherit `App`/`DelayedInit` indirectly, a `class`,
   *  `App` mixed with other parents, a self type, a body that uses `App`-specific
   *  API such as `executionStart`, member definitions interleaved with statements,
   *  an unsupported single-line body, and overlap with another active rewrite.
   */
  object AppToMain:
    private val mainHeader = "def main(args: Array[String]): Unit ="

    /** @param impl       the untyped template, used for the parent list and self type
     *  @param typedImpl  the typed template, used for the body split and semantic checks
     */
    def rewrite(cdef: untpd.TypeDef, impl: untpd.Template, typedImpl: tpd.Template, cls: ClassSymbol)(using Context): Unit =
      if !ctx.settings.YappToMain.value then return
      // Candidates are everything inheriting the deprecated `App`/`DelayedInit`,
      // so that even indirect inheritance is surfaced; only a direct `App` parent
      // is actually rewritten.
      val derivesApp = cls.derivesFrom(defn.AppClass)
      val derivesDelayedInit = cls.derivesFrom(defn.DelayedInitClass)

      if !derivesApp then
        // `DelayedInit` without `App` is not an application entry point, so there is
        // no `@main`-style rewrite to offer; the pattern must be refactored by hand.
        if derivesDelayedInit then
          report.warning(
            em"""$cls inherits from the deprecated `DelayedInit`, for which there is no automatic rewrite.
                |`DelayedInit` is not an application entry point; refactor the deferred-initialization pattern by hand (for example into an explicit method or the constructor body).""",
            cdef.srcPos)
        return

      def cannot(reason: String): Unit =
        report.warning(
          em"""$cls inherits from the deprecated `App` but could not be rewritten to an explicit `main` method: $reason.
              |Migrate it by hand, preferably to a `@main` method: https://docs.scala-lang.org/scala3/book/methods-main-methods.html""",
          cdef.srcPos)

      val directApp = typedImpl.parents.exists(_.tpe.classSymbol == defn.AppClass)
      // Objects carry a synthetic `_: Foo.type` self; a user-written `self =>`
      // would have a real name and sits inside the body, so it is out of scope.
      if !directApp then
        cannot("only an `object` that directly extends `App` can be rewritten")
      else if !cls.is(ModuleClass) then
        cannot("only an `object` can be rewritten")
      else if impl.self.name != nme.WILDCARD then
        cannot("it declares a self type")
      else if impl.parents.lengthIs != 1 then
        cannot("`App` is mixed with other parents")
      else
        // Keep only the user-written statements: an empty `{}` desugars to a spanless
        // synthetic `()`, and an inline def appends a synthetic accessor to the body;
        // filtering both leaves exactly the source layout the user wrote.
        val stmts = typedImpl.body.filter(t => t.span.exists && !t.symbol.is(Synthetic))

        // A safe split keeps a leading run of template members as object members and
        // moves the trailing statements into `main`. Besides definitions, `import`
        // and `export` are template-level constructs (exports add client-visible
        // members) and must stay in the object, never move into `main`.
        def isTemplateMember(t: tpd.Tree) =
          t.isInstanceOf[tpd.MemberDef] || t.isInstanceOf[tpd.ImportOrExport]
        val (leadingDefs, rest) = stmts.span(isTemplateMember)

        // A preserved member is unsafe if it references `App`-only API (where even
        // `args` is unavailable outside `main`) or overrides an `App`/`DelayedInit`
        // member (the `override` would dangle once the parent is dropped). Statements
        // moved into `main` may use the `args` parameter freely.
        if rest.exists(isTemplateMember) then
          cannot("its definitions, imports or exports are interleaved with statements, which would change their meaning")
        else if leadingDefs.exists(d => usesAppMember(d, allowArgs = false) || overridesAppApi(d)) then
          cannot("a member it keeps uses or overrides `App`-specific API such as `args`, `delayedInit` or `executionStart`")
        else if rest.exists(usesAppMember(_, allowArgs = true)) then
          cannot("its body uses `App`-specific API such as `executionStart`")
        else if stmts.nonEmpty && rest.isEmpty then
          cannot("its body has no statements to run in `main`")
        else
          // `rest` is the statements to wrap (empty only when the whole body is empty).
          wrapIntoMain(cdef, impl.parents.head, rest).foreach(cannot)

    /** Does `tree` reference an inherited `App` member that would no longer be
     *  available after the rewrite? When `allowArgs` is set, an unqualified `args`
     *  reference is exempt because statements moved into `main` see it as a parameter.
     *  Preserved members stay outside `main`, so for them `allowArgs` must be false.
     */
    private def usesAppMember(tree: tpd.Tree, allowArgs: Boolean)(using Context): Boolean =
      def isAppMember(sym: Symbol) = sym.exists && sym.maybeOwner == defn.AppClass
      tree.existsSubTree {
        case id: tpd.Ident   => isAppMember(id.symbol) && !(allowArgs && id.symbol.name.toString == "args")
        case sel: tpd.Select => isAppMember(sel.symbol)
        case _               => false
      }

    /** Does `tree`'s symbol override or implement a member of `App`/`DelayedInit`?
     *  Keeping such a member would leave an `override` of a parent the rewrite drops.
     */
    private def overridesAppApi(tree: tpd.Tree)(using Context): Boolean =
      val sym = tree.symbol
      sym.exists && sym.allOverriddenSymbols.exists: ov =>
        ov.owner == defn.AppClass || ov.owner == defn.DelayedInitClass

    /** Emit the patches that wrap `stmts` into a `main` method, dropping `extends App`.
     *  Returns `None` on success or `Some(reason)` if the source layout is not
     *  supported, so the caller can report it instead of silently doing nothing.
     */
    private def wrapIntoMain(cdef: untpd.TypeDef, appParent: untpd.Tree, stmts: List[tpd.Tree])(using Context): Option[String] =
      val src = ctx.source
      val content = src.content()
      val appStart = appParent.span.start
      val appEnd = appParent.span.end

      def isHSpace(c: Char) = c == ' ' || c == '\t'

      // Leading whitespace of the source line containing `offset`.
      def lineIndent(offset: Int): String =
        val ls = src.startOfLine(offset)
        var k = ls
        while k < content.length && isHSpace(content(k)) do k += 1
        String(content, ls, k - ls)

      // Is `offset` preceded on its line only by whitespace?
      def startsOwnLine(offset: Int): Boolean =
        (src.startOfLine(offset) until offset).forall(i => isHSpace(content(i)))

      // Start offset of ` extends App`, consuming the whitespace before `extends`.
      def extendsStart: Int =
        val kw = "extends"
        def matchesAt(i: Int) =
          i + kw.length <= content.length && kw.indices.forall(j => content(i + j) == kw(j))
        var i = appStart - kw.length
        while i >= 0 && !matchesAt(i) do i -= 1
        if i < 0 then -1
        else
          var s = i
          while s > 0 && content(s - 1).isWhitespace do s -= 1
          s

      // A directly `App`-extending object always has an `extends` keyword in source.
      val delStart = extendsStart
      assert(delStart >= 0, i"missing `extends` clause for $appParent")

      // First non-horizontal-space character after `App` decides the body style.
      var afterApp = appEnd
      while afterApp < content.length && isHSpace(content(afterApp)) do afterApp += 1
      val next = if afterApp < content.length then content(afterApp) else ' '

      val objIndent = lineIndent(cdef.span.start)

      // Bail out if another active rewrite (e.g. `-indent`/`-no-indent`) already
      // patches our region; emitting overlapping patches would crash writeBack.
      def conflicts(end: Int): Boolean = Rewrites.overlapsPatch(src, Span(delStart, end))
      val conflictReason =
        "it overlaps another active rewrite (for example `-indent`); run `-Yapp-to-main` without other rewrite options"

      if stmts.isEmpty then
        // The whole body is empty: replace ` extends App` (and any `{}`) with an
        // empty braceless `main`.
        val endRegion =
          if next == '{' then
            var k = afterApp + 1
            while k < content.length && content(k) != '}' do k += 1
            if k < content.length then k + 1 else appEnd
          else appEnd
        if conflicts(endRegion) then return Some(conflictReason)
        patch(Span(delStart, endRegion), s":\n$objIndent  $mainHeader ()")
        return None

      val firstStmt = stmts.head.span.start
      val lastStmt = stmts.last.span.end
      // We insert the `main` header at the first statement's line start and re-indent
      // by line, so a single-line body (`extends App { stmt }` / `extends App: stmt`,
      // where the statement shares its line with the brace/colon) is not supported.
      if !startsOwnLine(firstStmt) then
        return Some("its statements are not laid out on their own lines")

      val firstStmtLine = src.offsetToLine(firstStmt)
      val lastStmtLine = src.offsetToLine(lastStmt - 1)
      val bodyIndent = lineIndent(firstStmt)

      // Re-indent the statement lines after the first by two spaces (the first line's
      // extra indent is folded into the inserted `main` header to avoid two patches
      // sharing an offset).
      def reindentStmtTail(): Unit =
        var line = firstStmtLine + 1
        while line <= lastStmtLine do
          val ls = src.lineToOffset(line)
          var k = ls
          while k < content.length && isHSpace(content(k)) do k += 1
          if k < content.length && content(k) != '\n' then patch(Span(ls), "  ")
          line += 1

      next match
        case ':' =>
          if conflicts(lastStmt) then return Some(conflictReason)
          patch(Span(delStart, appEnd), "")
          patch(Span(src.startOfLine(firstStmt)), s"$bodyIndent$mainHeader\n  ")
          reindentStmtTail()
          None
        case '{' =>
          // Locate the object's closing brace and require it on its own line.
          var close = lastStmt
          while close < content.length && content(close) != '}' do close += 1
          if close >= content.length || src.offsetToLine(close) <= lastStmtLine then
            Some("its closing brace is not on its own line")
          else if conflicts(close + 1) then
            Some(conflictReason)
          else
            patch(Span(delStart, appEnd), "")
            patch(Span(src.startOfLine(firstStmt)), s"$bodyIndent$mainHeader {\n  ")
            reindentStmtTail()
            patch(Span(src.startOfLine(close)), s"$bodyIndent}\n")
            None
        case _ =>
          Some("its body layout is not supported")
    end wrapIntoMain

end Migrations
