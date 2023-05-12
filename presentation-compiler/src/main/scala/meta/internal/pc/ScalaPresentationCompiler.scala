package scala.meta.internal.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService
import java.{util as ju}

import scala.collection.JavaConverters.*
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.EmptyReportContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.ReportLevel
import scala.meta.internal.metals.StdReportContext
import scala.meta.internal.mtags.BuildInfo
import scala.meta.internal.pc.completions.CompletionProvider
import scala.meta.internal.pc.completions.OverrideCompletions
import scala.meta.pc.*

import dotty.tools.dotc.reporting.StoreReporter
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.TextEdit
import org.eclipse.{lsp4j as l}

case class ScalaPresentationCompiler(
    buildTargetIdentifier: String = "",
    classpath: Seq[Path] = Nil,
    options: List[String] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    ec: ExecutionContextExecutor = ExecutionContext.global,
    sh: Option[ScheduledExecutorService] = None,
    config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    folderPath: Option[Path] = None,
    reportsLevel: ReportLevel = ReportLevel.Info,
) extends PresentationCompiler:

  def this() = this("", Nil, Nil)

  val scalaVersion = BuildInfo.scalaCompilerVersion

  private val forbiddenOptions = Set("-print-lines", "-print-tasty")
  private val forbiddenDoubleOptions = Set("-release")
  given ReportContext =
    folderPath
      .map(StdReportContext(_, reportsLevel))
      .getOrElse(EmptyReportContext)

  override def withReportsLoggerLevel(level: String): PresentationCompiler =
    copy(reportsLevel = ReportLevel.fromString(level))

  val compilerAccess: CompilerAccess[StoreReporter, MetalsDriver] =
    Scala3CompilerAccess(
      config,
      sh,
      () => new Scala3CompilerWrapper(newDriver),
    )(using
      ec
    )

  private def removeDoubleOptions(options: List[String]): List[String] =
    options match
      case head :: _ :: tail if forbiddenDoubleOptions(head) =>
        removeDoubleOptions(tail)
      case head :: tail => head :: removeDoubleOptions(tail)
      case Nil => options

  def newDriver: MetalsDriver =
    val implicitSuggestionTimeout = List("-Ximport-suggestion-timeout", "0")
    val defaultFlags = List("-color:never")
    val filteredOptions = removeDoubleOptions(
      options.filterNot(forbiddenOptions)
    )
    val settings =
      filteredOptions ::: defaultFlags ::: implicitSuggestionTimeout ::: "-classpath" :: classpath
        .mkString(
          File.pathSeparator
        ) :: Nil
    new MetalsDriver(settings)

  override def semanticTokens(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Node]] =
    compilerAccess.withInterruptableCompiler(
      new ju.ArrayList[Node](),
      params.token(),
    ) { access =>
      val driver = access.compiler()
      new PcSemanticTokensProvider(driver, params).provide().asJava
    }

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean,
  ): CompletableFuture[String] =
    CompletableFuture.completedFuture {
      TastyUtils.getTasty(targetUri, isHttpEnabled)
    }

  def complete(params: OffsetParams): CompletableFuture[l.CompletionList] =
    compilerAccess.withInterruptableCompiler(
      EmptyCompletionList(),
      params.token,
    ) { access =>
      val driver = access.compiler()
      new CompletionProvider(
        search,
        driver,
        params,
        config,
        buildTargetIdentifier,
        folderPath,
      ).completions()

    }

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] =
    compilerAccess.withNonInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token,
    ) { access =>
      val driver = access.compiler()
      PcDefinitionProvider(driver, params, search).definitions()
    }

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    compilerAccess.withNonInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token,
    ) { access =>
      val driver = access.compiler()
      PcDefinitionProvider(driver, params, search).typeDefinitions()
    }

  def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[ju.List[DocumentHighlight]] =
    compilerAccess.withNonInterruptableCompiler(
      List.empty[DocumentHighlight].asJava,
      params.token,
    ) { access =>
      val driver = access.compiler()
      PcDocumentHighlightProvider(driver, params).highlights.asJava
    }

  def shutdown(): Unit =
    compilerAccess.shutdown()

  def restart(): Unit =
    compilerAccess.shutdownCurrentCompiler()

  def diagnosticsForDebuggingPurposes(): ju.List[String] =
    List[String]().asJava

  def semanticdbTextDocument(
      filename: URI,
      code: String,
  ): CompletableFuture[Array[Byte]] =
    compilerAccess.withNonInterruptableCompiler(
      Array.empty[Byte],
      EmptyCancelToken,
    ) { access =>
      val driver = access.compiler()
      val provider = SemanticdbTextDocumentProvider(driver, folderPath)
      provider.textDocument(filename, code)
    }

  def completionItemResolve(
      item: l.CompletionItem,
      symbol: String,
  ): CompletableFuture[l.CompletionItem] =
    compilerAccess.withNonInterruptableCompiler(
      item,
      EmptyCancelToken,
    ) { access =>
      val driver = access.compiler()
      CompletionItemResolver.resolve(item, symbol, search, config)(using
        driver.currentCtx
      )
    }

  def autoImports(
      name: String,
      params: scala.meta.pc.OffsetParams,
      isExtension: java.lang.Boolean,
  ): CompletableFuture[
    ju.List[scala.meta.pc.AutoImportsResult]
  ] =
    compilerAccess.withNonInterruptableCompiler(
      List.empty[scala.meta.pc.AutoImportsResult].asJava,
      params.token,
    ) { access =>
      val driver = access.compiler()
      new AutoImportsProvider(
        search,
        driver,
        name,
        params,
        config,
        buildTargetIdentifier,
      )
        .autoImports(isExtension)
        .asJava
    }

  def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      val driver = pc.compiler()
      OverrideCompletions.implementAllAt(
        params,
        driver,
        search,
        config,
      )
    }
  end implementAbstractMembers

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      new InferredTypeProvider(params, pc.compiler(), config, search)
        .inferredTypeEdits()
        .asJava
    }

  override def inlineValue(
      params: OffsetParams
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: Either[String, List[l.TextEdit]] = Right(List())
    (compilerAccess
      .withInterruptableCompiler(empty, params.token) { pc =>
        new PcInlineValueProviderImpl(pc.compiler(), params)
          .getInlineTextEdits()
      })
      .thenApply {
        case Right(edits: List[TextEdit]) => edits.asJava
        case Left(error: String) => throw new DisplayableException(error)
      }
  end inlineValue

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams,
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: ju.List[l.TextEdit] = new ju.ArrayList[l.TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, range.token) { pc =>
      new ExtractMethodProvider(
        range,
        extractionPos,
        pc.compiler(),
        search,
        options.contains("-no-indent"),
      )
        .extractMethod()
        .asJava
    }
  end extractMethod

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer],
  ): CompletableFuture[ju.List[l.TextEdit]] =
    val empty: Either[String, List[l.TextEdit]] = Right(List())
    (compilerAccess
      .withInterruptableCompiler(empty, params.token) { pc =>
        new ConvertToNamedArgumentsProvider(
          pc.compiler(),
          params,
          argIndices.asScala.map(_.toInt).toSet,
        ).convertToNamedArguments
      })
      .thenApplyAsync {
        case Left(error: String) => throw new DisplayableException(error)
        case Right(edits: List[l.TextEdit]) => edits.asJava
      }
  end convertToNamedArguments
  override def selectionRange(
      params: ju.List[OffsetParams]
  ): CompletableFuture[ju.List[l.SelectionRange]] =
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(List.empty[l.SelectionRange].asJava) {
        pc =>
          new SelectionRangeProvider(
            pc.compiler(),
            params,
          ).selectionRange().asJava
      }
    }
  end selectionRange

  def hover(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[HoverSignature]] =
    compilerAccess.withNonInterruptableCompiler(
      ju.Optional.empty[HoverSignature](),
      params.token,
    ) { access =>
      val driver = access.compiler()
      HoverProvider.hover(params, driver, search)
    }
  end hover

  def prepareRename(
      params: OffsetParams
  ): CompletableFuture[ju.Optional[l.Range]] =
    compilerAccess.withNonInterruptableCompiler(
      Optional.empty[l.Range](),
      params.token,
    ) { access =>
      val driver = access.compiler()
      Optional.ofNullable(
        PcRenameProvider(driver, params, None).prepareRename().orNull
      )
    }

  def rename(
      params: OffsetParams,
      name: String,
  ): CompletableFuture[ju.List[l.TextEdit]] =
    compilerAccess.withNonInterruptableCompiler(
      List[l.TextEdit]().asJava,
      params.token,
    ) { access =>
      val driver = access.compiler()
      PcRenameProvider(driver, params, Some(name)).rename().asJava
    }

  def newInstance(
      buildTargetIdentifier: String,
      classpath: ju.List[Path],
      options: ju.List[String],
  ): PresentationCompiler =
    copy(
      buildTargetIdentifier = buildTargetIdentifier,
      classpath = classpath.asScala.toSeq,
      options = options.asScala.toList,
    )

  def signatureHelp(params: OffsetParams): CompletableFuture[l.SignatureHelp] =
    compilerAccess.withNonInterruptableCompiler(
      new l.SignatureHelp(),
      params.token,
    ) { access =>
      val driver = access.compiler()
      SignatureHelpProvider.signatureHelp(driver, params, search)
    }

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[l.Diagnostic]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didClose(uri: URI): Unit =
    compilerAccess.withNonInterruptableCompiler(
      (),
      EmptyCancelToken,
    ) { access => access.compiler().close(uri) }

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

  override def isLoaded() = compilerAccess.isLoaded()

end ScalaPresentationCompiler
