package dotty.tools.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService
import java.util.Collections
import java.util as ju

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.jdk.CollectionConverters._
import scala.language.unsafeNulls
import scala.meta.internal.metals.ReportLevel
import scala.meta.internal.metals.StdReportContext
import scala.meta.internal.mtags.CommonMtagsEnrichments.*
import scala.meta.internal.pc.EmptySymbolSearch
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.*
import scala.meta.pc.{PcSymbolInformation as IPcSymbolInformation}

import dotty.tools.pc.completions.CompletionProvider
import dotty.tools.pc.completions.OverrideCompletions
import dotty.tools.pc.buildinfo.BuildInfo

import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l


/** Implementation of Presentation Compiler
 *
 *  NOTE: This class is not thread safe. Each consumer of this class should ensure that tasks are
 *  queued in sequence to guarantee correct caching.
 */
case class ScalaPresentationCompiler(
    buildTargetIdentifier: String = "",
    buildTargetName: Option[String] = None,
    classpath: Seq[Path] = Nil,
    options: List[String] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    ec: ExecutionContextExecutor = ExecutionContext.global,
    sh: Option[ScheduledExecutorService] = None,
    config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    folderPath: Option[Path] = None,
    reportsLevel: ReportLevel = ReportLevel.Info,
    completionItemPriority: CompletionItemPriority = (_: String) => 0,
) extends PresentationCompiler:

  override def supportedCodeActions(): ju.List[String] = List(
     CodeActionId.ConvertToNamedArguments,
     CodeActionId.ImplementAbstractMembers,
     CodeActionId.ExtractMethod,
     CodeActionId.InlineValue,
     CodeActionId.InsertInferredType,
     PcConvertToNamedLambdaParameters.codeActionId
   ).asJava

  def this() = this("", None, Nil, Nil)

  val scalaVersion = BuildInfo.scalaVersion

  private val forbiddenOptions = Set("-print-lines", "-print-tasty")
  private val forbiddenDoubleOptions = Set.empty[String]

  given reportContext: PcReportContext =
    folderPath
      .map(path => PcReportContext(StdReportContext(path, _ => buildTargetName, reportsLevel), additionalReportData))
      .getOrElse(PcReportContext.empty)

  override def codeAction[T](
    params: OffsetParams,
    codeActionId: String,
    codeActionPayload: Optional[T]
   ): CompletableFuture[ju.List[TextEdit]] =
     (codeActionId, codeActionPayload.asScala) match
        case (CodeActionId.ConvertToNamedArguments, Some(argIndices: ju.List[?])) =>
          val payload: ju.List[Integer] =
            argIndices.asScala.collect { case i: Integer => i.asInstanceOf[Integer] }.asJava
          convertToNamedArguments(params, payload)
        case (CodeActionId.ImplementAbstractMembers, _) =>
          implementAbstractMembers(params)
        case (CodeActionId.InsertInferredType, _) =>
          insertInferredType(params)
        case (CodeActionId.InlineValue, _) =>
          inlineValue(params)
        case (CodeActionId.ExtractMethod, Some(extractionPos: OffsetParams)) =>
          params match {
            case range: RangeParams =>
              extractMethod(range, extractionPos)
            case _ => failedFuture(new IllegalArgumentException(s"Expected range parameters"))
          }
        case (PcConvertToNamedLambdaParameters.codeActionId, _) =>
          driverAccess.enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
            PcConvertToNamedLambdaParameters(driver, params).convertToNamedLambdaParameters
        case (id, _) => failedFuture(new IllegalArgumentException(s"Unsupported action id $id"))

  private def failedFuture[T](e: Throwable): CompletableFuture[T] =
    val f = new CompletableFuture[T]()
    f.completeExceptionally(e)
    f

  override def withCompletionItemPriority(
    priority: CompletionItemPriority
  ): PresentationCompiler =
    copy(completionItemPriority = priority)

  override def withBuildTargetName(buildTargetName: String) =
    copy(buildTargetName = Some(buildTargetName))

  override def withReportsLoggerLevel(level: String): PresentationCompiler =
    copy(reportsLevel = ReportLevel.fromString(level))

  val driverSettings =
    val implicitSuggestionTimeout = List("-Ximport-suggestion-timeout", "0")
    val defaultFlags = List("-color:never")
    val filteredOptions = removeDoubleOptions(options.filterNot(forbiddenOptions))

    filteredOptions ::: defaultFlags ::: implicitSuggestionTimeout ::: "-classpath" :: classpath
      .mkString(File.pathSeparator) :: Nil

  lazy val driverAccess = DriverAccess(config, driverSettings, reportContext)

  private def removeDoubleOptions(options: List[String]): List[String] =
    options match
      case head :: _ :: tail if forbiddenDoubleOptions(head) =>
        removeDoubleOptions(tail)
      case head :: tail => head :: removeDoubleOptions(tail)
      case Nil => options

  override def semanticTokens(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Node]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new PcSemanticTokensProvider(driver, params)
          .provide()
          .asJava

  override def inlayHints(
      params: InlayHintsParams
  ): ju.concurrent.CompletableFuture[ju.List[l.InlayHint]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new PcInlayHintsProvider(driver, params, search)
          .provide()
          .asJava

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] =
    CompletableFuture.completedFuture:
      TastyUtils.getTasty(targetUri, isHttpEnabled)

  def complete(params: OffsetParams): CompletableFuture[l.CompletionList] =
    val (wasCursorApplied, completionText) = CompletionProvider.applyCompletionCursor(params)
    val inputs = CompilationInputs(params.uri.nn, completionText, params.token().nn, wasCursorApplied)

    driverAccess
      .enqueueCancellable(inputs): driver =>
        new CompletionProvider(
          search,
          driver,
          driverSettings,
          params,
          config,
          buildTargetIdentifier,
          folderPath,
          completionItemPriority,
          wasCursorApplied
        ).completions()

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params.nn)): driver =>
        PcDefinitionProvider(driver, params, search).definitions()

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        PcDefinitionProvider(driver, params, search).typeDefinitions()

  def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[ju.List[DocumentHighlight]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        PcDocumentHighlightProvider(driver, params).highlights.asJava

  override def references(
      params: ReferencesRequest
  ): CompletableFuture[ju.List[ReferencesResult]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params.file)): driver =>
        PcReferencesProvider(driver, params)
          .references()
          .asJava

  def inferExpectedType(params: OffsetParams): CompletableFuture[ju.Optional[String]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new InferExpectedType(search, driver, params).infer().asJava

  def shutdown(): Unit = driverAccess.shutdown()

  def restart(): Unit = driverAccess.restart()

  def diagnosticsForDebuggingPurposes(): ju.List[String] = Collections.emptyList()

  override def info(
      symbol: String
  ): CompletableFuture[Optional[IPcSymbolInformation]] =
    driverAccess
      .lookup: driver =>
        SymbolInformationProvider(using driver.currentCtx)
          .info(symbol)
          .map(_.asJava)
          .asJava

  def semanticdbTextDocument(
      filename: URI,
      code: String
  ): CompletableFuture[Array[Byte]] =
    driverAccess
      .enqueueCancellable(CompilationInputs(filename, code)): driver =>
        SemanticdbTextDocumentProvider(driver, folderPath)
          .textDocument(filename, code)

  def completionItemResolve(
      item: l.CompletionItem,
      symbol: String
  ): CompletableFuture[l.CompletionItem] =
    driverAccess.lookup: driver =>
      CompletionItemResolver.resolve(item, symbol, search, config)(using driver.currentCtx)

  def autoImports(
      name: String,
      params: scala.meta.pc.OffsetParams,
      isExtension: java.lang.Boolean
  ): CompletableFuture[ju.List[scala.meta.pc.AutoImportsResult]] =
    driverAccess.enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
      new AutoImportsProvider(
        search,
        driver,
        name,
        params,
        config,
        buildTargetIdentifier
      )
        .autoImports(isExtension)
        .asJava

  def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        OverrideCompletions.implementAllAt(
          params,
          driver,
          search,
          config
        )

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new InferredTypeProvider(params, driver, config, search)
          .inferredTypeEdits()
          .asJava

  override def inlineValue(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new PcInlineValueProvider(driver, params)
          .getInlineTextEdits()
      .thenApply:
        case Right(edits: List[TextEdit]) => edits.asJava
        case Left(error: String) => throw new DisplayableException(error)

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(range)): driver =>
        new ExtractMethodProvider(
          range,
          extractionPos,
          driver,
          search,
          options.contains("-no-indent"),
        )
          .extractMethod()
          .asJava

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer]
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        new ConvertToNamedArgumentsProvider(
          driver,
          params,
          argIndices.asScala.map(_.toInt).toSet
        ).convertToNamedArguments
      .thenApply:
        case Left(error: String) => throw new DisplayableException(error)
        case Right(edits: List[l.TextEdit]) => edits.asJava

  override def selectionRange(
      params: ju.List[OffsetParams]
  ): CompletableFuture[ju.List[l.SelectionRange]] =
    if params.isEmpty then
      CompletableFuture.completedFuture(Collections.emptyList())
    else
      driverAccess.enqueueCancellable(CompilationInputs.fromParams(params.asScala.head)): driver =>
        new SelectionRangeProvider(driver, params).selectionRange().asJava

  def hover(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[HoverSignature]] =
    driverAccess
      .enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
        HoverProvider.hover(params, driver, search, config.hoverContentType())

  def prepareRename(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[l.Range]] =
    driverAccess.enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
      PcRenameProvider(driver, params, None).prepareRename().asJava

  def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[ju.List[l.TextEdit]] =
    driverAccess.enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
      PcRenameProvider(driver, params, Some(name)).rename().asJava

  def newInstance(
      buildTargetIdentifier: String,
      classpath: ju.List[Path],
      options: ju.List[String]
  ): PresentationCompiler =
    copy(
      buildTargetIdentifier = buildTargetIdentifier,
      classpath = classpath.asScala.toSeq,
      options = options.asScala.toList
    )

  def signatureHelp(params: OffsetParams): CompletableFuture[l.SignatureHelp] =
    driverAccess.enqueueCancellable(CompilationInputs.fromParams(params)): driver =>
      SignatureHelpProvider.signatureHelp(driver, params, search)

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[l.Diagnostic]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didClose(uri: URI): Unit =
    driverAccess.lookup(_.close(uri))

  override def withExecutorService(
      executorService: ExecutorService
  ): PresentationCompiler =
    copy(ec = ExecutionContext.fromExecutorService(executorService))

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): PresentationCompiler =
    copy(config = config)

  override def withScheduledExecutorService(
      sh: ScheduledExecutorService
  ): PresentationCompiler =
    copy(sh = Some(sh))

  def withSearch(search: SymbolSearch): PresentationCompiler =
    copy(search = search)

  def withWorkspace(workspace: Path): PresentationCompiler =
    copy(folderPath = Some(workspace))

  override def isLoaded() = true

  private def additionalReportData =
    Map(
      "scalaVersion" -> scalaVersion,
      "classpath" -> classpath
         .map(path => s"$path [ ${if path.exists then "exists" else "missing"} ]")
         .mkString(", "),
      "options" -> options.mkString(" ")
    )

end ScalaPresentationCompiler
