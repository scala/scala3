package dotty.tools.pc
package completions

import java.nio.file.Path

import scala.jdk.CollectionConverters.*
import scala.meta.pc.CompletionItemPriority
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.DerivedName
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.profile.Profiler
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.AutoImports.AutoImportEdits
import dotty.tools.pc.AutoImports.AutoImportsGenerator
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.InsertTextFormat
import org.eclipse.lsp4j.InsertTextMode
import org.eclipse.lsp4j.Range as LspRange
import org.eclipse.lsp4j.TextEdit

object CompletionProvider:
  val allKeywords =
    val softKeywords = Tokens.softModifierNames + nme.as + nme.derives + nme.extension + nme.throws + nme.using
    Tokens.keywords.toList.map(Tokens.tokenString) ++ softKeywords.map(_.toString)

class CompletionProvider(
    search: SymbolSearch,
    cachingDriver: InteractiveDriver,
    freshDriver: () => InteractiveDriver,
    params: OffsetParams,
    config: PresentationCompilerConfig,
    buildTargetIdentifier: String,
    folderPath: Option[Path],
    referenceCounter: CompletionItemPriority
)(using reports: ReportContext):
  def completions(): CompletionList =
    val uri = params.uri().nn
    val text = params.text().nn

    val (wasCursorApplied, code) = applyCompletionCursor(params)
    val sourceFile = SourceFile.virtual(uri, code)

    /** Creating a new fresh driver is way slower than reusing existing one, but
     *  runnig a compilation has side effects that modifies the state of the
     *  driver. We don't want to affect cachingDriver state with compilation
     *  including "CURSOR" suffix.
     *
     *  We could in theory save this fresh driver for reuse, but it is a choice
     *  between extra memory usage and speed. The scenario in which "CURSOR" is
     *  applied (empty query or query equal to any keyword) has a slim chance of
     *  happening.
     */

    val driver = if wasCursorApplied then freshDriver() else cachingDriver
    driver.run(uri, sourceFile)

    given ctx: Context = driver.currentCtx
    val pos = driver.sourcePosition(params)
    val (items, isIncomplete) = driver.compilationUnits.get(uri) match
      case Some(unit) =>
        val newctx = ctx.fresh
          .setCompilationUnit(unit)
          .setProfiler(Profiler()(using ctx))
          .withPhase(Phases.typerPhase(using ctx))
        val tpdPath0 = Interactive.pathTo(unit.tpdTree, pos.span)(using newctx)
        val adjustedPath = Interactive.resolveTypedOrUntypedPath(tpdPath0, pos)(using newctx)

        val tpdPath = tpdPath0 match
          case Select(qual, name) :: tail
              /** If for any reason we end up in param after lifting, we want to
               *  inline the synthetic val:
               *  ```
               *  List(1).iterator.sliding@@
               *  ```
               *  will be transformed into:
               *  ```
               *  1| val $1$: Iterator[Int] = List.apply[Int]([1 : Int]*).iterator
               *  2| {
               *  3|   def $anonfun(size: Int, step: Int): $1$.GroupedIterator[Int] =
               *  4|     $1$.sliding[Int](size, step)
               *  5|   closure($anonfun)
               *  6| }:((Int, Int) => Iterator[Int]#GroupedIterator[Int])
               *  ```
               *  With completion being run at line 4 at @@:
               *  ```
               *  4| $1$.sliding@@[Int](size, step)
               *  ```
               */
              if qual.symbol.is(
                Flags.Synthetic
              ) && qual.span.isZeroExtent && qual.symbol.name.isInstanceOf[DerivedName] =>
            qual.symbol.defTree match
              case valdef: ValDef if !valdef.rhs.isEmpty => Select(valdef.rhs, name) :: tail
              case _ => tpdPath0
          case _ => tpdPath0

        val locatedCtx = Interactive.contextOfPath(tpdPath)(using newctx)
        val indexedCtx = IndexedContext(pos)(using locatedCtx)

        val completionPos = CompletionPos.infer(pos, params, adjustedPath, wasCursorApplied)(using locatedCtx)

        val autoImportsGen = AutoImports.generator(
          completionPos.toSourcePosition,
          text,
          unit.tpdTree,
          unit.comments,
          indexedCtx,
          config
        )

        val (completions, searchResult) =
          new Completions(
            text,
            locatedCtx,
            search,
            buildTargetIdentifier,
            completionPos,
            indexedCtx,
            tpdPath,
            adjustedPath,
            config,
            folderPath,
            autoImportsGen,
            unit.comments,
            driver.settings,
            referenceCounter
          ).completions()

        val items = completions.zipWithIndex.map { case (item, idx) =>
          completionItems(
            item,
            idx,
            autoImportsGen,
            completionPos,
            tpdPath,
            indexedCtx
          )(using locatedCtx)
        }
        val isIncomplete = searchResult match
          case SymbolSearch.Result.COMPLETE => false
          case SymbolSearch.Result.INCOMPLETE => true
        (items, isIncomplete)
      case None => (Nil, false)

    new CompletionList(
      isIncomplete,
      items.asJava
    )
  end completions

  /** In case if completion comes from empty line like:
   *  {{{
   *  class Foo:
   *    val a = 1
   *    @@
   *  }}}
   *  it's required to modify actual code by additional Ident.
   *
   *  Otherwise, completion poisition doesn't point at any tree because scala
   *  parser trim end position to the last statement pos.
   */
  private def applyCompletionCursor(params: OffsetParams): (Boolean, String) =
    val text = params.text().nn
    val offset = params.offset()
    val query = Completion.naiveCompletionPrefix(text, offset)
    def isValidLastChar =
      val lastChar = text.charAt(offset - 1)
      lastChar.isUnicodeIdentifierPart || lastChar == '.'
    if offset > 0 && isValidLastChar && !CompletionProvider.allKeywords.contains(query) then false -> text
    else
      val isStartMultilineComment =

        val i = params.offset()
        i >= 3 && (text.charAt(i - 1) match
          case '*' =>
            text.charAt(i - 2) == '*' &&
            text.charAt(i - 3) == '/'
          case _ => false)
      true -> (
        if isStartMultilineComment then
          // Insert potentially missing `*/` to avoid comment out all codes after the "/**".
          text.substring(0, offset).nn + Cursor.value + "*/" + text.substring(offset)
        else
          text.substring(0, offset).nn + Cursor.value + text.substring(offset)
      )

  private def completionItems(
      completion: CompletionValue,
      idx: Int,
      autoImports: AutoImportsGenerator,
      completionPos: CompletionPos,
      path: List[Tree],
      indexedContext: IndexedContext
  )(using ctx: Context): CompletionItem =
    val printer =
      ShortenedTypePrinter(search, IncludeDefaultParam.ResolveLater)(using indexedContext)

    val underlyingCompletion = completion match
      case CompletionValue.ExtraMethod(_, underlying) => underlying
      case other => other

    // For overloaded signatures we get multiple symbols, so we need
    // to recalculate the description
    // related issue https://github.com/lampepfl/scala3/issues/11941
    lazy val kind: CompletionItemKind = underlyingCompletion.completionItemKind
    val description = underlyingCompletion.description(printer)
    val label =
      if config.isDetailIncludedInLabel then completion.labelWithDescription(printer)
      else completion.label
    val ident = underlyingCompletion.insertText.getOrElse(underlyingCompletion.label)
    lazy val isInStringInterpolation =
      path match
        // s"My name is $name"
        case (_: Ident) :: (_: SeqLiteral) :: (_: Typed) :: Apply(
              Select(Apply(Select(Select(_, name), _), _), _),
              _
            ) :: _ =>
          name == nme.StringContext
        // "My name is $name"
        case Literal(Constant(_: String)) :: _ =>
          true
        case _ =>
          false

    def wrapInBracketsIfRequired(newText: String): String =
      if underlyingCompletion.snippetAffix.nonEmpty && isInStringInterpolation then
        "{" + newText + "}"
      else newText

    def mkItem(
        newText: String,
        additionalEdits: List[TextEdit] = Nil,
        range: Option[LspRange] = None
    ): CompletionItem =
      val oldText = params.text().nn.substring(completionPos.queryStart, completionPos.identEnd)
      val trimmedNewText =
        var nt = newText
        if completionPos.hasLeadingBacktick then nt = nt.stripPrefix("`")
        if completionPos.hasTrailingBacktick then nt = nt.stripSuffix("`")
        nt

      val editRange = if trimmedNewText.startsWith(oldText) then completionPos.stripSuffixEditRange
      else completionPos.toEditRange

      val textEdit = new TextEdit(range.getOrElse(editRange), wrapInBracketsIfRequired(trimmedNewText))

      val item = new CompletionItem(label)
      item.setSortText(f"${idx}%05d")
      item.setDetail(description)
      item.setFilterText(underlyingCompletion.filterText.getOrElse(underlyingCompletion.label))
      item.setTextEdit(textEdit)
      item.setAdditionalTextEdits((underlyingCompletion.additionalEdits ++ additionalEdits).asJava)
      underlyingCompletion.insertMode.foreach(item.setInsertTextMode)

      val data = underlyingCompletion.completionData(buildTargetIdentifier)
      item.setData(data.toJson)

      item.setTags(underlyingCompletion.lspTags.asJava)

      if config.isCompletionSnippetsEnabled() then
        item.setInsertTextFormat(InsertTextFormat.Snippet)

      underlyingCompletion.command.foreach { command =>
        item.setCommand(new Command("", command))
      }

      item.setKind(kind)
      item
    end mkItem

    val completionTextSuffix = underlyingCompletion.snippetAffix.toSuffix
    val completionTextPrefix = underlyingCompletion.snippetAffix.toInsertPrefix

    lazy val backtickSoftKeyword = path match
      case (_: Select) :: _ => false
      case _ => true

    def mkItemWithImports(
        v: CompletionValue.Workspace | CompletionValue.Extension |
          CompletionValue.Interpolator | CompletionValue.ImplicitClass
    ) =
      val sym = v.symbol
      path match
        case (_: Ident) :: (_: Import) :: _ =>
          mkItem(sym.fullNameBackticked(backtickSoftKeyword = false))
        case _ =>
          autoImports.editsForSymbol(v.importSymbol) match
            case Some(edits) =>
              edits match
                case AutoImportEdits(Some(nameEdit), other) =>
                  mkItem(nameEdit.getNewText().nn, other.toList, range = Some(nameEdit.getRange().nn))
                case _ =>
                  mkItem(
                    v.insertText.getOrElse(
                      completionTextPrefix + ident.backticked(backtickSoftKeyword) + completionTextSuffix
                    ),
                    edits.edits,
                    range = v.range
                  )
            case None =>
              val r = indexedContext.lookupSym(sym)
              r match
                case IndexedContext.Result.InScope =>
                  mkItem(
                    v.insertText.getOrElse(
                      completionTextPrefix + ident.backticked(backtickSoftKeyword) + completionTextSuffix
                    ),
                    range = v.range
                  )
                // Special case when symbol is out of scope, and there is no auto import.
                // It means that it will use fully qualified path
                case _ if isInStringInterpolation =>
                  mkItem(
                    "{" + completionTextPrefix + sym.fullNameBackticked + completionTextSuffix + "}",
                    range = v.range
                  )
                case _ if v.isExtensionMethod =>
                  mkItem(
                    completionTextPrefix + ident.backticked(backtickSoftKeyword) + completionTextSuffix,
                    range = v.range
                  )
                case _ =>
                  mkItem(
                    completionTextPrefix + sym.fullNameBackticked(
                      backtickSoftKeyword
                    ) + completionTextSuffix,
                    range = v.range
                  )
          end match

    underlyingCompletion match
      case v: (CompletionValue.Workspace | CompletionValue.Extension | CompletionValue.ImplicitClass) =>
        mkItemWithImports(v)
      case v: CompletionValue.Interpolator if v.isWorkspace || v.isExtension =>
        mkItemWithImports(v)
      case _ =>
        val nameText = underlyingCompletion.insertText.getOrElse(ident.backticked(backtickSoftKeyword))
        val nameWithAffixes = completionTextPrefix + nameText + completionTextSuffix
        mkItem(nameWithAffixes, range = underlyingCompletion.range)
  end completionItems
end CompletionProvider
