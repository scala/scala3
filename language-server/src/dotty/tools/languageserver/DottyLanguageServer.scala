package dotty.tools
package languageserver

import java.net.URI
import java.io._
import java.nio.file._
import java.util.concurrent.CompletableFuture
import java.util.function.Function

import com.fasterxml.jackson.databind.ObjectMapper

import org.eclipse.lsp4j

import scala.collection._
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import scala.io.Codec

import dotc._
import ast.{Trees, tpd}
import core._, core.Decorators.{sourcePos => _, _}
import Contexts._, Flags._, Names._, NameOps._, Symbols._, SymDenotations._, Trees._, Types._
import classpath.ClassPathEntries
import reporting._, reporting.diagnostic.MessageContainer
import util._
import interactive._, interactive.InteractiveDriver._

import languageserver.config.ProjectConfig

import lsp4j.services._

/** An implementation of the Language Server Protocol for Dotty.
 *
 *  You should not have to directly this class, instead see `dotty.tools.languageserver.Main`.
 *
 *  For more information see:
 *  - The LSP is defined at https://github.com/Microsoft/language-server-protocol
 *  - This implementation is based on the LSP4J library: https://github.com/eclipse/lsp4j
 */
class DottyLanguageServer extends LanguageServer
    with LanguageClientAware with TextDocumentService with WorkspaceService { thisServer =>
  import ast.tpd._

  import DottyLanguageServer._
  import InteractiveDriver._

  import lsp4j.jsonrpc.{CancelChecker, CompletableFutures}
  import lsp4j.jsonrpc.messages.{Either => JEither}
  import lsp4j._


  private[this] var rootUri: String = _
  private[this] var client: LanguageClient = _

  private[this] var myDrivers: mutable.Map[ProjectConfig, InteractiveDriver] = _

  def drivers: Map[ProjectConfig, InteractiveDriver] = thisServer.synchronized {
    if (myDrivers == null) {
      assert(rootUri != null, "`drivers` cannot be called before `initialize`")
      val configFile = new File(new URI(rootUri + '/' + IDE_CONFIG_FILE))
      val configs: List[ProjectConfig] = (new ObjectMapper).readValue(configFile, classOf[Array[ProjectConfig]]).toList

      val defaultFlags = List(/*"-Yplain-printer","-Yprintpos"*/)

      myDrivers = new mutable.HashMap
      for (config <- configs) {
        val classpathFlags = List("-classpath", (config.classDirectory +: config.dependencyClasspath).mkString(File.pathSeparator))
        val settings = defaultFlags ++ config.compilerArguments.toList ++ classpathFlags
        myDrivers.put(config, new InteractiveDriver(settings))
      }
    }
    myDrivers
  }

  /** The driver instance responsible for compiling `uri` */
  def driverFor(uri: URI): InteractiveDriver = {
    val matchingConfig =
      drivers.keys.find(config => config.sourceDirectories.exists(sourceDir =>
        new File(uri.getPath).getCanonicalPath.startsWith(sourceDir.getCanonicalPath)))
    matchingConfig match {
      case Some(config) =>
        drivers(config)
      case None =>
        val config = drivers.keys.head
        println(s"No configuration contains $uri as a source file, arbitrarily choosing ${config.id}")
        drivers(config)
    }
  }

  override def connect(client: LanguageClient): Unit = {
    this.client = client
  }

  override def exit(): Unit = {
    System.exit(0)
  }

  override def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(new Object)
  }

  private[this] def computeAsync[R](fun: CancelChecker => R): CompletableFuture[R] =
    CompletableFutures.computeAsync({(cancelToken: CancelChecker) =>
      // We do not support any concurrent use of the compiler currently.
      thisServer.synchronized {
        cancelToken.checkCanceled()
        try {
          fun(cancelToken)
        } catch {
          case NonFatal(ex) =>
            ex.printStackTrace
            throw ex
        }
      }
    })

  override def initialize(params: InitializeParams) = computeAsync { cancelToken =>
    rootUri = params.getRootUri
    assert(rootUri != null)

    val c = new ServerCapabilities
    c.setTextDocumentSync(TextDocumentSyncKind.Full)
    c.setDocumentHighlightProvider(true)
    c.setDocumentSymbolProvider(true)
    c.setDefinitionProvider(true)
    c.setRenameProvider(true)
    c.setHoverProvider(true)
    c.setWorkspaceSymbolProvider(true)
    c.setReferencesProvider(true)
    c.setCompletionProvider(new CompletionOptions(
      /* resolveProvider = */ false,
      /* triggerCharacters = */ List(".").asJava))

    // Do most of the initialization asynchronously so that we can return early
    // from this method and thus let the client know our capabilities.
    CompletableFuture.supplyAsync(() => drivers)
      .exceptionally {
        // Can't use a function literal here because of #2367
        new Function[Throwable, Nothing] {
          def apply(ex: Throwable) = {
            ex.printStackTrace
            sys.exit(1)
          }
        }
      }

    new InitializeResult(c)
  }

  override def didOpen(params: DidOpenTextDocumentParams): Unit = thisServer.synchronized {
    val document = params.getTextDocument
    val uri = new URI(document.getUri)
    val driver = driverFor(uri)

    val text = document.getText
    val diags = driver.run(uri, text)

    client.publishDiagnostics(new PublishDiagnosticsParams(
      document.getUri,
      diags.flatMap(diagnostic).asJava))
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = thisServer.synchronized {
    val document = params.getTextDocument
    val uri = new URI(document.getUri)
    val driver = driverFor(uri)

    val change = params.getContentChanges.get(0)
    assert(change.getRange == null, "TextDocumentSyncKind.Incremental support is not implemented")

    val text = change.getText
    val diags = driver.run(uri, text)

    client.publishDiagnostics(new PublishDiagnosticsParams(
      document.getUri,
      diags.flatMap(diagnostic).asJava))
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = thisServer.synchronized {
    val document = params.getTextDocument
    val uri = new URI(document.getUri)

    driverFor(uri).close(uri)
  }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    /*thisServer.synchronized*/ {}

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    /*thisServer.synchronized*/ {}

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    /*thisServer.synchronized*/ {}


  // FIXME: share code with messages.NotAMember
  override def completion(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val items = Interactive.completions(driver.openedTrees(uri), pos)._2

    JEither.forRight(new CompletionList(
      /*isIncomplete = */ false, items.map(completionItem).asJava))
  }

  override def definition(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val sym = Interactive.enclosingSourceSymbol(driver.openedTrees(uri), pos)

    if (sym == NoSymbol) Nil.asJava
    else {
      // This returns the position of sym as well as the overrides of sym, but
      // for performance we only look for overrides in the file where sym is
      // defined.
      // We need a configuration option to choose how "go to definition" should
      // behave with respect to overriding and overriden definitions, ideally
      // this should be part of the LSP protocol.
      val trees = SourceTree.fromSymbol(sym.topLevelClass.asClass).toList
      val defs = Interactive.namedTrees(trees, includeReferences = false, includeOverriden = true, sym)
      defs.map(d => location(d.namePos)).asJava
    }
  }

  override def references(params: ReferenceParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val includeDeclaration = params.getContext.isIncludeDeclaration
    val pos = sourcePosition(driver, uri, params.getPosition)
    val sym = Interactive.enclosingSourceSymbol(driver.openedTrees(uri), pos)

    if (sym == NoSymbol) Nil.asJava
    else {
      // FIXME: this will search for references in all trees on the classpath, but we really
      // only need to look for trees in the target directory if the symbol is defined in the
      // current project
      val trees = driver.allTrees
      val refs = Interactive.namedTrees(trees, includeReferences = true, (tree: tpd.NameTree) =>
        (includeDeclaration || !Interactive.isDefinition(tree))
          && Interactive.matchSymbol(tree, sym, includeOverriden = true))

      refs.map(ref => location(ref.namePos)).asJava
    }
  }

  override def rename(params: RenameParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val sym = Interactive.enclosingSourceSymbol(driver.openedTrees(uri), pos)

    if (sym == NoSymbol) new WorkspaceEdit()
    else {
      val trees = driver.allTrees
      val linkedSym = sym.linkedClass
      val newName = params.getNewName

      val refs = Interactive.namedTrees(trees, includeReferences = true, tree =>
        (Interactive.matchSymbol(tree, sym, includeOverriden = true)
          || (linkedSym != NoSymbol && Interactive.matchSymbol(tree, linkedSym, includeOverriden = true))))

      val changes = refs.groupBy(ref => toUri(ref.source).toString).mapValues(_.map(ref => new TextEdit(range(ref.namePos), newName)).asJava)

      new WorkspaceEdit(changes.asJava)
    }
  }

  override def documentHighlight(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val uriTrees = driver.openedTrees(uri)
    val sym = Interactive.enclosingSourceSymbol(uriTrees, pos)

    if (sym == NoSymbol) Nil.asJava
    else {
      val refs = Interactive.namedTrees(uriTrees, includeReferences = true, includeOverriden = true, sym)
      refs.map(ref => new DocumentHighlight(range(ref.namePos), DocumentHighlightKind.Read)).asJava
    }
  }

  override def hover(params: TextDocumentPositionParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val pos = sourcePosition(driver, uri, params.getPosition)
    val tp = Interactive.enclosingType(driver.openedTrees(uri), pos)
    val tpw = tp.widenTermRefExpr

    if (tpw == NoType) new Hover
    else {
      val str = tpw.show.toString
      new Hover(List(JEither.forLeft(str)).asJava, null)
    }
  }

  override def documentSymbol(params: DocumentSymbolParams) = computeAsync { cancelToken =>
    val uri = new URI(params.getTextDocument.getUri)
    val driver = driverFor(uri)
    implicit val ctx = driver.currentCtx

    val uriTrees = driver.openedTrees(uri)

    val defs = Interactive.namedTrees(uriTrees, includeReferences = false, _ => true)
    defs.map(d => symbolInfo(d.tree.symbol, d.namePos)).asJava
  }

  override def symbol(params: WorkspaceSymbolParams) = computeAsync { cancelToken =>
    val query = params.getQuery

    drivers.values.toList.flatMap { driver =>
      implicit val ctx = driver.currentCtx

      val trees = driver.allTrees
      val defs = Interactive.namedTrees(trees, includeReferences = false, nameSubstring = query)
      defs.map(d => symbolInfo(d.tree.symbol, d.namePos))
    }.asJava
  }

  override def getTextDocumentService: TextDocumentService = this
  override def getWorkspaceService: WorkspaceService = this

  // Unimplemented features. If you implement one of them, you may need to add a
  // capability in `initialize`
  override def codeAction(params: CodeActionParams) = null
  override def codeLens(params: CodeLensParams) = null
  override def formatting(params: DocumentFormattingParams) = null
  override def rangeFormatting(params: DocumentRangeFormattingParams) = null
  override def onTypeFormatting(params: DocumentOnTypeFormattingParams) = null
  override def resolveCodeLens(params: CodeLens) = null
  override def resolveCompletionItem(params: CompletionItem) = null
  override def signatureHelp(params: TextDocumentPositionParams) = null
}

object DottyLanguageServer {
  /** Configuration file normally generated by sbt-dotty */
  final val IDE_CONFIG_FILE = ".dotty-ide.json"

  /** Convert an lsp4j.Position to a SourcePosition */
  def sourcePosition(driver: InteractiveDriver, uri: URI, pos: lsp4j.Position): SourcePosition = {
    val source = driver.openedFiles(uri)
    if (source.exists) {
      val p = Positions.Position(source.lineToOffset(pos.getLine) + pos.getCharacter)
      new SourcePosition(source, p)
    }
    else NoSourcePosition
  }

  /** Convert a SourcePosition to an lsp4j.Range */
  def range(p: SourcePosition): lsp4j.Range =
    new lsp4j.Range(
      new lsp4j.Position(p.startLine, p.startColumn),
      new lsp4j.Position(p.endLine, p.endColumn)
    )

  /** Convert a SourcePosition to an lsp4.Location */
  def location(p: SourcePosition): lsp4j.Location =
    new lsp4j.Location(toUri(p.source).toString, range(p))

  /** Convert a MessageContainer to an lsp4j.Diagnostic */
  def diagnostic(mc: MessageContainer): Option[lsp4j.Diagnostic] =
    if (!mc.pos.exists)
      None // diagnostics without positions are not supported: https://github.com/Microsoft/language-server-protocol/issues/249
    else {
      def severity(level: Int): lsp4j.DiagnosticSeverity = {
        import interfaces.{Diagnostic => D}
        import lsp4j.{DiagnosticSeverity => DS}

        level match {
          case D.INFO =>
            DS.Information
          case D.WARNING =>
            DS.Warning
          case D.ERROR =>
            DS.Error
        }
      }

      val code = mc.contained().errorId.errorNumber.toString
      Some(new lsp4j.Diagnostic(
        range(mc.pos), mc.message, severity(mc.level), /*source =*/ "", code))
    }

  /** Create an lsp4j.CompletionItem from a Symbol */
  def completionItem(sym: Symbol)(implicit ctx: Context): lsp4j.CompletionItem = {
    def completionItemKind(sym: Symbol)(implicit ctx: Context): lsp4j.CompletionItemKind = {
      import lsp4j.{CompletionItemKind => CIK}

      if (sym.is(Package))
        CIK.Module // No CompletionItemKind.Package (https://github.com/Microsoft/language-server-protocol/issues/155)
      else if (sym.isConstructor)
        CIK.Constructor
      else if (sym.isClass)
        CIK.Class
      else if (sym.is(Mutable))
        CIK.Variable
      else if (sym.is(Method))
        CIK.Method
      else
        CIK.Field
    }

    val label = sym.name.show.toString
    val item = new lsp4j.CompletionItem(label)
    item.setDetail(sym.info.widenTermRefExpr.show.toString)
    item.setKind(completionItemKind(sym))
    item
  }

  /** Create an lsp4j.SymbolInfo from a Symbol and a SourcePosition */
  def symbolInfo(sym: Symbol, pos: SourcePosition)(implicit ctx: Context): lsp4j.SymbolInformation = {
    def symbolKind(sym: Symbol)(implicit ctx: Context): lsp4j.SymbolKind = {
      import lsp4j.{SymbolKind => SK}

      if (sym.is(Package))
        SK.Package
      else if (sym.isConstructor)
        SK.Constructor
      else if (sym.isClass)
        SK.Class
      else if (sym.is(Mutable))
        SK.Variable
      else if (sym.is(Method))
        SK.Method
      else
        SK.Field
    }

    val name = sym.name.show.toString
    val containerName =
      if (sym.owner.exists && !sym.owner.isEmptyPackage)
        sym.owner.name.show.toString
      else
        null

    new lsp4j.SymbolInformation(name, symbolKind(sym), location(pos), containerName)
  }
}
