package dotty.tools.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService
import java.util as ju

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.PcQueryContext
import scala.meta.internal.metals.ReportLevel
import scala.meta.internal.mtags.CommonMtagsEnrichments.*
import scala.meta.internal.pc.CompilerAccess
import scala.meta.internal.pc.DefinitionResultImpl
import scala.meta.internal.pc.EmptyCompletionList
import scala.meta.internal.pc.EmptySymbolSearch
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.*
import scala.meta.pc.PcSymbolInformation as IPcSymbolInformation
import scala.meta.pc.reports.EmptyReportContext
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.pc.InferExpectedType
import dotty.tools.pc.SymbolInformationProvider
import dotty.tools.pc.buildinfo.BuildInfo
import dotty.tools.pc.completions.CompletionProvider
import dotty.tools.pc.completions.OverrideCompletions

import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l

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
    reportContext: ReportContext = EmptyReportContext()
) extends PresentationCompiler:

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

  def this() = this("", None, Nil, Nil)

  val scalaVersion = BuildInfo.scalaVersion

  private val forbiddenOptions = Set("-print-tasty")
  private val forbiddenDoubleOptions = Set.empty[String]

  override def codeAction[T](
      params: OffsetParams,
      codeActionId: String,
      codeActionPayload: Optional[T]
  ): CompletableFuture[ju.List[TextEdit]] =
    (codeActionId, codeActionPayload.asScala) match
      case (
            CodeActionId.ConvertToNamedArguments,
            Some(argIndices: ju.List[?])
          ) =>
        val payload =
          argIndices.asScala.collect { case i: Integer => i.toInt }.toSet
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
          case _ => failedFuture(new IllegalArgumentException(s"Expected range parameters"))
      case (PcConvertToNamedLambdaParameters.codeActionId, _) =>
        compilerAccess.withNonInterruptableCompiler(List.empty[l.TextEdit].asJava, params.token) {
          access => PcConvertToNamedLambdaParameters(access.compiler(), params).convertToNamedLambdaParameters
        }(params.toQueryContext)
      case (id, _) => failedFuture(new IllegalArgumentException(s"Unsupported action id $id"))

  private def failedFuture[T](e: Throwable): CompletableFuture[T] =
    val f = new CompletableFuture[T]()
    f.completeExceptionally(e)
    f

  override def withCompletionItemPriority(
      priority: CompletionItemPriority
  ): PresentationCompiler =
    copy(completionItemPriority = priority)

  override def withBuildTargetName(buildTargetName: String): PresentationCompiler =
    copy(buildTargetName = Some(buildTargetName))

  override def withReportsLoggerLevel(level: String): PresentationCompiler =
    copy(reportsLevel = ReportLevel.fromString(level))

  val compilerAccess: CompilerAccess[StoreReporter, InteractiveDriver] =
    Scala3CompilerAccess(
      config,
      sh,
      () => new Scala3CompilerWrapper(CachingDriver(driverSettings))
    )(using ec)

  val driverSettings =
    val implicitSuggestionTimeout = List("-Ximport-suggestion-timeout", "0")
    val defaultFlags = List("-color:never")
    val filteredOptions = removeDoubleOptions(options.filterNot(forbiddenOptions))

    filteredOptions ::: defaultFlags ::: implicitSuggestionTimeout ::: "-classpath" :: classpath
      .mkString(File.pathSeparator) :: Nil

  private def removeDoubleOptions(options: List[String]): List[String] =
    options match
      case head :: _ :: tail if forbiddenDoubleOptions(head) =>
        removeDoubleOptions(tail)
      case head :: tail => head :: removeDoubleOptions(tail)
      case Nil => options

  override def semanticTokens(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Node]] =
    compilerAccess.withInterruptableCompiler(
      new ju.ArrayList[Node](),
      params.token()
    ) { access =>
      val driver = access.compiler()
      new PcSemanticTokensProvider(driver, params).provide().asJava
    }(params.toQueryContext)

  override def inlayHints(
      params: InlayHintsParams
  ): ju.concurrent.CompletableFuture[ju.List[l.InlayHint]] =
    compilerAccess.withInterruptableCompiler(
      new ju.ArrayList[l.InlayHint](),
      params.token()
    ) { access =>
      val driver = access.compiler()
      new PcInlayHintsProvider(driver, params, search)
        .provide()
        .asJava
    }(params.toQueryContext)

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] =
    CompletableFuture.completedFuture {
      TastyUtils.getTasty(targetUri, isHttpEnabled)
    }

  def complete(params: OffsetParams): CompletableFuture[l.CompletionList] =
    compilerAccess.withInterruptableCompiler(
      EmptyCompletionList(),
      params.token()
    ) { access =>
      val driver = access.compiler()
      new CompletionProvider(
        search,
        driver,
        () => InteractiveDriver(driverSettings),
        params,
        config,
        buildTargetIdentifier,
        folderPath,
        completionItemPriority
      ).completions()
    }(params.toQueryContext)

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] =
    compilerAccess.withInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token()
    ) { access =>
      val driver = access.compiler()
      PcDefinitionProvider(driver, params, search).definitions()
    }(params.toQueryContext)

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    compilerAccess.withInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token()
    ) { access =>
      val driver = access.compiler()
      PcDefinitionProvider(driver, params, search).typeDefinitions()
    }(params.toQueryContext)

  def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[ju.List[DocumentHighlight]] =
    compilerAccess.withInterruptableCompiler(
      List.empty[DocumentHighlight].asJava,
      params.token()
    ) { access =>
      val driver = access.compiler()
      PcDocumentHighlightProvider(driver, params).highlights.asJava
    }(params.toQueryContext)

  override def references(
      params: ReferencesRequest
  ): CompletableFuture[ju.List[ReferencesResult]] =
    compilerAccess.withNonInterruptableCompiler(
      List.empty[ReferencesResult].asJava,
      params.file().token
    ) { access =>
      val driver = access.compiler()
      PcReferencesProvider(driver, params)
        .references()
        .asJava
    }(params.file().toQueryContext)

  def inferExpectedType(params: OffsetParams): CompletableFuture[ju.Optional[String]] =
    compilerAccess.withInterruptableCompiler(
      Optional.empty(),
      params.token
    ) { access =>
      val driver = access.compiler()
      new InferExpectedType(search, driver, params).infer().asJava
    }(params.toQueryContext)

  def shutdown(): Unit =
    compilerAccess.shutdown()

  def restart(): Unit =
    compilerAccess.shutdownCurrentCompiler()

  def diagnosticsForDebuggingPurposes(): ju.List[String] =
    List[String]().asJava

  override def info(
      symbol: String
  ): CompletableFuture[Optional[IPcSymbolInformation]] =
    compilerAccess.withNonInterruptableCompiler[Optional[IPcSymbolInformation]](
      Optional.empty(),
      EmptyCancelToken
    ) { access =>
      SymbolInformationProvider(using access.compiler().currentCtx)
        .info(symbol)
        .map(_.asJava)
        .asJava
    }(emptyQueryContext)

  def semanticdbTextDocument(
      filename: URI,
      code: String
  ): CompletableFuture[Array[Byte]] =
    val virtualFile = CompilerVirtualFileParams(filename, code)
    compilerAccess.withNonInterruptableCompiler(
      Array.empty[Byte],
      EmptyCancelToken
    ) { access =>
      val driver = access.compiler()
      val provider = SemanticdbTextDocumentProvider(driver, folderPath)
      provider.textDocument(filename, code)
    }(virtualFile.toQueryContext)

  def completionItemResolve(
      item: l.CompletionItem,
      symbol: String
  ): CompletableFuture[l.CompletionItem] =
    compilerAccess.withNonInterruptableCompiler(
      item,
      EmptyCancelToken
    ) { access =>
      val driver = access.compiler()
      CompletionItemResolver.resolve(item, symbol, search, config)(using driver.currentCtx)
    }(emptyQueryContext)

  def autoImports(
      name: String,
      params: scala.meta.pc.OffsetParams,
      isExtension: java.lang.Boolean
  ): CompletableFuture[
    ju.List[scala.meta.pc.AutoImportsResult]
  ] =
    compilerAccess.withNonInterruptableCompiler(
      List.empty[scala.meta.pc.AutoImportsResult].asJava,
      params.token()
    ) { access =>
      val driver = access.compiler()
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
    }(params.toQueryContext)

  def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withNonInterruptableCompiler(
      empty,
      params.token()
    ) { pc =>
      val driver = pc.compiler()
      OverrideCompletions.implementAllAt(
        params,
        driver,
        search,
        config
      )
    }(params.toQueryContext)

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withNonInterruptableCompiler(
      empty,
      params.token()
    ) { pc =>
      new InferredTypeProvider(params, pc.compiler(), config, search)
        .inferredTypeEdits()
        .asJava
    }(params.toQueryContext)

  def insertInferredMethod(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withNonInterruptableCompiler(
      empty,
      params.token()
    ) { pc =>
      new InferredMethodProvider(params, pc.compiler(), config, search)
        .inferredMethodEdits()
        .asJava
    }(params.toQueryContext)

  override def inlineValue(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: Either[String, List[l.TextEdit]] = Right(List())
    (compilerAccess
      .withInterruptableCompiler(empty, params.token()) { pc =>
        new PcInlineValueProvider(pc.compiler(), params)
          .getInlineTextEdits()
      }(params.toQueryContext))
      .thenApply {
        case Right(edits: List[TextEdit]) => edits.asJava
        case Left(error: String) => throw new DisplayableException(error)
      }

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, range.token()) {
      pc =>
        new ExtractMethodProvider(
          range,
          extractionPos,
          pc.compiler(),
          search,
          options.contains("-no-indent")
        )
          .extractMethod()
          .asJava
    }(range.toQueryContext)

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer]
  ): CompletableFuture[ju.List[l.TextEdit]] =
    convertToNamedArguments(params, argIndices.asScala.toSet.map(_.toInt))

  def convertToNamedArguments(
      params: OffsetParams,
      argIndices: Set[Int]
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: Either[String, List[l.TextEdit]] = Right(List())
    (compilerAccess
      .withNonInterruptableCompiler(empty, params.token()) { pc =>
        new ConvertToNamedArgumentsProvider(
          pc.compiler(),
          params,
          argIndices
        ).convertToNamedArguments
      }(params.toQueryContext))
      .thenApplyAsync {
        case Left(error: String) => throw new DisplayableException(error)
        case Right(edits: List[l.TextEdit]) => edits.asJava
      }
  override def selectionRange(
      params: ju.List[OffsetParams]
  ): CompletableFuture[ju.List[l.SelectionRange]] =
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(
        List.empty[l.SelectionRange].asJava
      ) { pc =>
        new SelectionRangeProvider(
          pc.compiler(),
          params
        ).selectionRange().asJava
      }(params.asScala.headOption.map(_.toQueryContext).getOrElse(emptyQueryContext))
    }
  end selectionRange

  def hover(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[HoverSignature]] =
    compilerAccess.withNonInterruptableCompiler(
      ju.Optional.empty[HoverSignature](),
      params.token()
    ) { access =>
      val driver = access.compiler()
      HoverProvider.hover(params, driver, search, config.hoverContentType())
    }(params.toQueryContext)
  end hover

  def prepareRename(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[l.Range]] =
    compilerAccess.withNonInterruptableCompiler(
      Optional.empty[l.Range](),
      params.token()
    ) { access =>
      val driver = access.compiler()
      Optional.ofNullable(
        PcRenameProvider(driver, params, None).prepareRename().orNull
      )
    }(params.toQueryContext)

  def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[ju.List[l.TextEdit]] =
    compilerAccess.withNonInterruptableCompiler(
      List[l.TextEdit]().asJava,
      params.token()
    ) { access =>
      val driver = access.compiler()
      PcRenameProvider(driver, params, Some(name)).rename().asJava
    }(params.toQueryContext)

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
    compilerAccess.withNonInterruptableCompiler(
      new l.SignatureHelp(),
      params.token()
    ) { access =>
      val driver = access.compiler()
      SignatureHelpProvider.signatureHelp(driver, params, search)
    }(params.toQueryContext)

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[l.Diagnostic]] =
    compilerAccess.withNonInterruptableCompiler(
      ju.Collections.emptyList(),
      EmptyCancelToken
    ) { access =>
      val driver = access.compiler()
      DiagnosticProvider(driver, params).diagnostics().asJava
    }(params.toQueryContext)

  override def didClose(uri: URI): Unit =
    compilerAccess.withNonInterruptableCompiler(
      (),
      EmptyCancelToken
    ) { access => access.compiler().close(uri) }(emptyQueryContext)

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

  override def withReportContext(reportContext: ReportContext): PresentationCompiler =
    copy(reportContext = reportContext)

  def withWorkspace(workspace: Path): PresentationCompiler =
    copy(folderPath = Some(workspace))

  override def isLoaded() = compilerAccess.isLoaded()

  def additionalReportData() =
    s"""|Scala version: $scalaVersion
        |Classpath:
        |${classpath
        .map(path => s"$path [${if path.exists then "exists" else "missing"} ]")
        .mkString(", ")}
        |Options:
        |${options.mkString(" ")}
        |""".stripMargin

  extension (params: VirtualFileParams)
    def toQueryContext = PcQueryContext(Some(params), additionalReportData)

  def emptyQueryContext = PcQueryContext(None, additionalReportData)

end ScalaPresentationCompiler
