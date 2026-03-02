package dotty.tools.pc.completions

import scala.collection.mutable.ListBuffer
import scala.meta.internal.pc.CompletionFuzzy
import scala.meta.internal.pc.InterpolationSplice
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.pc.CompilerSearchVisitor
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

object InterpolatorCompletions:

  def contribute(
      text: String,
      completionPos: CompletionPos,
      indexedContext: IndexedContext,
      lit: Literal,
      path: List[Tree],
      completions: Completions,
      snippetsEnabled: Boolean,
      search: SymbolSearch,
      config: PresentationCompilerConfig,
      buildTargetIdentifier: String
  )(using Context, ReportContext) =
    InterpolationSplice(completionPos.queryEnd, text.toCharArray().nn, text) match
      case Some(interpolator) =>
        InterpolatorCompletions.contributeScope(
          text,
          lit,
          completionPos,
          interpolator,
          indexedContext,
          completions,
          snippetsEnabled,
          hasStringInterpolator =
            path.tail.headOption.exists(_.isInstanceOf[SeqLiteral]),
          search,
          buildTargetIdentifier
        )
      case None =>
        InterpolatorCompletions.contributeMember(
          lit,
          path,
          text,
          completionPos,
          completions,
          snippetsEnabled,
          search,
          buildTargetIdentifier
        )
    end match

  /** Find the identifier that corresponds to the previous interpolation splice.
   *  For string `s" $Main.metho@@ "` we want to get `Main` identifier. The
   *  difference with Scala 2 is that we search for it through the path using
   *  the created partial function.
   */
  private def interpolatorMemberArg(
      lit: Literal,
      parent: Tree
  ): PartialFunction[Tree, Option[Ident | Select]] =
    case tree @ Apply(
          _,
          List(Typed(expr: SeqLiteral, _))
        ) if expr.elems.exists {
          case _: Ident => true
          case _: Select => true
          case _ => false
        } =>
      parent match
        case SeqLiteral(elems, _) if elems.size > 0 =>
          expr.elems.zip(elems.tail).collectFirst {
            case (i: (Ident | Select), literal) if literal == lit =>
              i
          }
        case _ => None

  /** A completion to select type members inside string interpolators.
   *
   *  Example:
   *  ```
   *    // before
   *    s"Hello $name.len@@!"
   *    // after
   *    s"Hello ${name.length()$0}"
   *  ```
   */
  private def contributeMember(
      lit: Literal,
      path: List[Tree],
      text: String,
      completionPos: CompletionPos,
      completions: Completions,
      areSnippetsSupported: Boolean,
      search: SymbolSearch,
      buildTargetIdentifier: String
  )(using Context, ReportContext): List[CompletionValue] =
    def newText(
        label: String,
        affix: CompletionAffix,
        identOrSelect: Ident | Select
    ): String =
      val snippetCursor = suffixEnding(affix.toSuffixOpt, areSnippetsSupported)
      new StringBuilder()
        .append('{')
        .append(affix.toPrefix) // we use toPrefix here, because previous prefix is added in the next step
        .append(text.substring(identOrSelect.span.start, identOrSelect.span.end))
        .append('.')
        .append(label.backticked)
        .append(snippetCursor)
        .append('}')
        .toString

    def extensionMethods(qualType: Type) =
      val buffer = ListBuffer.empty[Symbol]
      val visitor = new CompilerSearchVisitor(sym =>
        if sym.is(ExtensionMethod) &&
          qualType.widenDealias <:< sym.extensionParam.info.widenDealias
        then
          buffer.append(sym)
          true
        else false
      )
      search.searchMethods(completionPos.query, buildTargetIdentifier, visitor)
      buffer.toList

    def completionValues(
        syms: Seq[Symbol],
        isExtension: Boolean,
        identOrSelect: Ident | Select
    ): Seq[CompletionValue] =
      syms.collect {
        case sym
            if CompletionFuzzy.matches(
              completionPos.query,
              sym.name.toString()
            ) =>
          val label = sym.name.decoded
          completions.completionsWithAffix(
            sym,
            label,
            (name, denot, affix) =>
              CompletionValue.Interpolator(
                denot.symbol,
                label,
                Some(newText(name, affix, identOrSelect)),
                Nil,
                Some(completionPos.originalCursorPosition.withStart(identOrSelect.span.start).toLsp),
                // Needed for VS Code which will not show the completion otherwise
                Some(identOrSelect.name.toString() + "." + label),
                denot.symbol,
                isExtension = isExtension
              )
          )
      }.flatten

    val qualType =
      for
        parent <- path.tail.headOption.toList
        if lit.span.exists && text.charAt(lit.span.point - 1) != '}'
        identOrSelect <- path
          .collectFirst(interpolatorMemberArg(lit, parent))
          .flatten
      yield identOrSelect

    qualType.flatMap(identOrSelect =>
      val tp = identOrSelect.symbol.info
      val members = tp.allMembers.map(_.symbol)
      val extensionSyms = extensionMethods(tp)
      completionValues(members, isExtension = false, identOrSelect) ++
        completionValues(extensionSyms, isExtension = true, identOrSelect)
    )
  end contributeMember

  private def suffixEnding(
      suffix: Option[String],
      areSnippetsSupported: Boolean
  ): String =
    suffix match
      case Some(suffix) if areSnippetsSupported && suffix == "()" =>
        suffix + "$0"
      case Some(suffix) => suffix
      case None if areSnippetsSupported => "$0"
      case _ => ""

  /** contributeScope provides completions to convert a string literal into
   *  splice, example `"Hello $na@@"`.
   *
   *  When converting a string literal into an interpolator we need to ensure a
   *  few cases:
   *
   *    - escape existing `$` characters into `$$`, which are printed as `\$\$`
   *      in order to escape the TextMate snippet syntax.
   *    - wrap completed name in curly braces `s"Hello ${name}_` when the
   *      trailing character can be treated as an identifier part.
   *    - insert the leading `s` interpolator.
   *    - place the cursor at the end of the completed name using TextMate `$0`
   *      snippet syntax.
   */
  private def contributeScope(
      text: String,
      lit: Literal,
      completionPos: CompletionPos,
      interpolator: InterpolationSplice,
      indexedContext: IndexedContext,
      completions: Completions,
      areSnippetsSupported: Boolean,
      hasStringInterpolator: Boolean,
      search: SymbolSearch,
      buildTargetIdentifier: String
  )(using ctx: Context, reportsContext: ReportContext): List[CompletionValue] =
    val litStartPos = lit.span.start
    val litEndPos = lit.span.end - (if completionPos.withCURSOR then Cursor.value.length else 0)
    val position = completionPos.originalCursorPosition
    val span = position.span
    val nameStart =
      span.withStart(span.start - interpolator.name.size)
    val nameRange = position.withSpan(nameStart).toLsp
    val hasClosingBrace: Boolean = text.charAt(span.point) == '}'
    val hasOpeningBrace: Boolean = text.charAt(
      span.start - interpolator.name.size - 1
    ) == '{'

    def additionalEdits(): List[l.TextEdit] =
      val interpolatorEdit =
        if !hasStringInterpolator then
          val range = lit.sourcePos.withEnd(litStartPos).toLsp
          List(new l.TextEdit(range, "s"))
        else Nil
      val dollarEdits =
        for
          i <- litStartPos to litEndPos
          if !hasStringInterpolator &&
            text.charAt(i) == '$' && i != interpolator.dollar
        yield new l.TextEdit(lit.sourcePos.focusAt(i).toLsp, "$")
      interpolatorEdit ++ dollarEdits

    def newText(symbolName: String, affix: CompletionAffix): String =
      val out = new StringBuilder()
      val identifier = symbolName.backticked
      val symbolNeedsBraces =
        interpolator.needsBraces ||
          identifier.startsWith("`") ||
          affix.toSuffixOpt.isDefined ||
          affix.toPrefix.nonEmpty
      if symbolNeedsBraces && !hasOpeningBrace then out.append('{')
      out.append(affix.toInsertPrefix)
      out.append(identifier)
      out.append(suffixEnding(affix.toSuffixOpt, areSnippetsSupported))
      if symbolNeedsBraces && !hasClosingBrace then out.append('}')
      out.toString

    val workspaceSymbols = ListBuffer.empty[Symbol]
    val visitor = new CompilerSearchVisitor(sym =>
      indexedContext.lookupSym(sym) match
        case IndexedContext.Result.InScope => false
        case _ =>
          if sym.is(Flags.Module) then workspaceSymbols += sym
          true
    )
    if interpolator.name.nonEmpty then
      search.search(interpolator.name, buildTargetIdentifier, visitor)

    def collectCompletions(
        isWorkspace: Boolean
    ): PartialFunction[Symbol, List[CompletionValue]] =
      case sym
          if CompletionFuzzy.matches(
            interpolator.name,
            sym.name.decoded
          ) && !sym.isType =>
        val label = sym.name.decoded
        completions.completionsWithAffix(
          sym,
          label,
          (name, denot, affix) =>
            CompletionValue.Interpolator(
              denot.symbol,
              label,
              Some(newText(name, affix)),
              additionalEdits(),
              Some(nameRange),
              None,
              sym,
              isWorkspace
            )
        )

    val fromWorkspace =
      workspaceSymbols.toList.collect(collectCompletions(isWorkspace = true))
    val fromLocal = indexedContext.scopeSymbols.collect(
      collectCompletions(isWorkspace = false)
    )
    (fromLocal ++ fromWorkspace).flatten
  end contributeScope

end InterpolatorCompletions
