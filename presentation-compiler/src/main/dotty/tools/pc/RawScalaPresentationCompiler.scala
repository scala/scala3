package dotty.tools.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.Optional
import java.util as ju

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.PcQueryContext
import scala.meta.internal.metals.ReportLevel
import scala.meta.internal.mtags.CommonMtagsEnrichments.*
import scala.meta.internal.pc.EmptySymbolSearch
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.*
import scala.meta.pc.PcSymbolInformation as IPcSymbolInformation
import scala.meta.pc.reports.EmptyReportContext
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.pc.InferExpectedType
import dotty.tools.pc.SymbolInformationProvider
import dotty.tools.pc.buildinfo.BuildInfo
import dotty.tools.pc.completions.CompletionProvider
import dotty.tools.pc.completions.OverrideCompletions

import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l

/** The raw public API of the presentation compiler that does not handle
 *  synchronisation. Scala compiler can't run concurrent code at that point, so
 *  we need to enforce sequential, single threaded execution.
 *
 *  It has to be implemented by the consumer of this API.
 */
case class RawScalaPresentationCompiler(
    buildTargetIdentifier: String = "",
    buildTargetName: Option[String] = None,
    classpath: Seq[Path] = Nil,
    options: List[String] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    folderPath: Option[Path] = None,
    reportsLevel: ReportLevel = ReportLevel.Info,
    completionItemPriority: CompletionItemPriority = (_: String) => 0,
    reportContext: ReportContext = EmptyReportContext()
) extends RawPresentationCompiler:

  def this() = this("uninitialized-presentation-compiler")

  given ReportContext = reportContext

  override def supportedCodeActions(): ju.List[String] = List(
    CodeActionId.ConvertToNamedArguments,
    CodeActionId.ImplementAbstractMembers,
    CodeActionId.ExtractMethod,
    CodeActionId.InlineValue,
    CodeActionId.InsertInferredType,
    CodeActionId.InsertInferredMethod,
    PcConvertToNamedLambdaParameters.codeActionId,
  ).asJava

  override val scalaVersion = BuildInfo.scalaVersion

  private val forbiddenOptions = Set("-print-tasty")
  private val forbiddenDoubleOptions = Set.empty[String]

  val driverSettings =
    val implicitSuggestionTimeout = List("-Ximport-suggestion-timeout", "0")
    val defaultFlags = List("-color:never")
    val filteredOptions = removeDoubleOptions(options.filterNot(forbiddenOptions))

    filteredOptions ::: defaultFlags ::: implicitSuggestionTimeout ::: "-classpath" :: classpath
      .mkString(File.pathSeparator) :: Nil

  lazy val driver: InteractiveDriver = CachingDriver(driverSettings)

  override def codeAction[T](
      params: OffsetParams,
      codeActionId: String,
      codeActionPayload: Optional[T]
  ): ju.List[TextEdit] =
    (codeActionId, codeActionPayload.asScala) match
      case (
            CodeActionId.ConvertToNamedArguments,
            Some(argIndices: ju.List[?])
          ) =>
        val payload = argIndices.asScala.collect { case i: Integer => i.toInt }.toSet
        convertToNamedArguments(params, payload)
      case (CodeActionId.ImplementAbstractMembers, _) =>
        implementAbstractMembers(params)
      case (CodeActionId.InsertInferredType, _) =>
        insertInferredType(params)
      case (CodeActionId.InsertInferredMethod, _) =>
        insertInferredMethod(params)
      case (CodeActionId.InlineValue, _) =>
        inlineValue(params)
      case (CodeActionId.ExtractMethod, Some(extractionPos: OffsetParams)) =>
        params match
          case range: RangeParams =>
            extractMethod(range, extractionPos)
          case _ => throw new IllegalArgumentException(s"Expected range parameters")
      case (PcConvertToNamedLambdaParameters.codeActionId, _) =>
        PcConvertToNamedLambdaParameters(driver, params).convertToNamedLambdaParameters
      case (id, _) => throw new IllegalArgumentException(s"Unsupported action id $id")

  override def withCompletionItemPriority(
      priority: CompletionItemPriority
  ): RawPresentationCompiler =
    copy(completionItemPriority = priority)

  override def withBuildTargetName(buildTargetName: String): RawPresentationCompiler =
    copy(buildTargetName = Some(buildTargetName))

  override def withReportsLoggerLevel(level: String): RawPresentationCompiler =
    copy(reportsLevel = ReportLevel.fromString(level))

  private def removeDoubleOptions(options: List[String]): List[String] =
    options match
      case head :: _ :: tail if forbiddenDoubleOptions(head) =>
        removeDoubleOptions(tail)
      case head :: tail => head :: removeDoubleOptions(tail)
      case Nil => options

  override def semanticTokens(
      params: VirtualFileParams
  ): ju.List[Node] =
    PcSemanticTokensProvider(driver, params).provide().asJava

  override def inlayHints(
      params: InlayHintsParams
  ): ju.List[l.InlayHint] =
    PcInlayHintsProvider(driver, params, search)
      .provide()
      .asJava

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): String =
    TastyUtils.getTasty(targetUri, isHttpEnabled)

  override def complete(params: OffsetParams, completionTriggerKind: l.CompletionTriggerKind): l.CompletionList =
    CompletionProvider(
      search,
      driver,
      () => InteractiveDriver(driverSettings),
      params,
      config,
      buildTargetIdentifier,
      folderPath,
      completionItemPriority
    ).completions()

  def definition(params: OffsetParams): DefinitionResult =
    PcDefinitionProvider(driver, params, search).definitions()

  override def typeDefinition(
      params: OffsetParams
  ): DefinitionResult =
    PcDefinitionProvider(driver, params, search).typeDefinitions()

  override def documentHighlight(
      params: OffsetParams
  ): ju.List[DocumentHighlight] =
    PcDocumentHighlightProvider(driver, params).highlights.asJava

  override def references(
      params: ReferencesRequest
  ): ju.List[ReferencesResult] =
    PcReferencesProvider(driver, params)
      .references()
      .asJava

  def inferExpectedType(params: OffsetParams): ju.Optional[String] =
    InferExpectedType(search, driver, params).infer().asJava

  override def info(
      symbol: String
  ): Optional[IPcSymbolInformation] =
    SymbolInformationProvider(using driver.currentCtx)
      .info(symbol)
      .map(_.asJava)
      .asJava

  override def semanticdbTextDocument(
      filename: URI,
      code: String
  ): Array[Byte] =
    val provider = SemanticdbTextDocumentProvider(driver, folderPath)
    provider.textDocument(filename, code)

  override def completionItemResolve(
      item: l.CompletionItem,
      symbol: String
  ): l.CompletionItem =
    CompletionItemResolver.resolve(item, symbol, search, config)(using driver.currentCtx) // FIX ME

  override def autoImports(
      name: String,
      params: scala.meta.pc.OffsetParams,
      isExtension: java.lang.Boolean
  ): ju.List[scala.meta.pc.AutoImportsResult] =
    AutoImportsProvider(
      search,
      driver,
      name,
      params,
      config,
      buildTargetIdentifier
    )
      .autoImports(isExtension)
      .asJava

  override def implementAbstractMembers(
      params: OffsetParams
  ): ju.List[l.TextEdit] =
    OverrideCompletions.implementAllAt(
      params,
      driver,
      search,
      config
    )

  override def insertInferredType(
      params: OffsetParams
  ): ju.List[l.TextEdit] =
    InferredTypeProvider(params, driver, config, search)
      .inferredTypeEdits()
      .asJava

  private def insertInferredMethod(
      params: OffsetParams
  ): ju.List[l.TextEdit] =
    InferredMethodProvider(params, driver, config, search)
      .inferredMethodEdits()
      .asJava

  override def inlineValue(
      params: OffsetParams
  ): ju.List[l.TextEdit] =
    PcInlineValueProvider(driver, params).getInlineTextEdits() match
      case Right(edits: List[TextEdit]) => edits.asJava
      case Left(error: String) => throw new DisplayableException(error)

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): ju.List[l.TextEdit] =
    ExtractMethodProvider(
      range,
      extractionPos,
      driver,
      search,
      options.contains("-no-indent")
    )
      .extractMethod()
      .asJava

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer]
  ): ju.List[l.TextEdit] =
    convertToNamedArguments(params, argIndices.asScala.toSet.map(_.toInt))

  def convertToNamedArguments(
      params: OffsetParams,
      argIndices: Set[Int]
  ): ju.List[l.TextEdit] =
    ConvertToNamedArgumentsProvider(
      driver,
      params,
      argIndices
    ).convertToNamedArguments match
      case Left(error: String) => throw new DisplayableException(error)
      case Right(edits: List[l.TextEdit]) => edits.asJava

  override def selectionRange(
      params: ju.List[OffsetParams]
  ): ju.List[l.SelectionRange] =
    SelectionRangeProvider(
      driver,
      params
    ).selectionRange().asJava

  override def hover(
      params: OffsetParams
  ): ju.Optional[HoverSignature] =
    HoverProvider.hover(params, driver, search, config.hoverContentType())

  override def prepareRename(
      params: OffsetParams
  ): ju.Optional[l.Range] =
    PcRenameProvider(driver, params, None).prepareRename().asJava

  override def rename(
      params: OffsetParams,
      name: String
  ): ju.List[l.TextEdit] =
    PcRenameProvider(driver, params, Some(name)).rename().asJava

  override def newInstance(
      buildTargetIdentifier: String,
      classpath: ju.List[Path],
      options: ju.List[String]
  ): RawPresentationCompiler =
    copy(
      buildTargetIdentifier = buildTargetIdentifier,
      classpath = classpath.asScala.toSeq,
      options = options.asScala.toList
    )

  override def signatureHelp(params: OffsetParams): l.SignatureHelp =
    SignatureHelpProvider.signatureHelp(driver, params, search)

  override def didChange(params: VirtualFileParams): ju.List[l.Diagnostic] =
    DiagnosticProvider(driver, params).diagnostics().asJava

  override def didClose(uri: URI): Unit =
    driver.close(uri)

  override def semanticdbTextDocument(params: VirtualFileParams): Array[Byte] =
    semanticdbTextDocument(params.uri, params.text)

  override def buildTargetId(): String = buildTargetIdentifier

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): RawPresentationCompiler =
    copy(config = config)

  override def withSearch(search: SymbolSearch): RawPresentationCompiler =
    copy(search = search)

  override def withReportContext(reportContext: ReportContext): RawPresentationCompiler =
    copy(reportContext = reportContext)

  override def withWorkspace(workspace: Path): RawPresentationCompiler =
    copy(folderPath = Some(workspace))

end RawScalaPresentationCompiler
