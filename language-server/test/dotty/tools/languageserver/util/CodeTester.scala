package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.actions._
import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.{TestFile, TestServer}
import dotty.tools.dotc.reporting.ErrorMessageID
import dotty.tools.dotc.util.Signatures.Signature

import org.eclipse.lsp4j.{ CompletionItem, CompletionItemKind, DocumentHighlightKind, Diagnostic, DiagnosticSeverity }
import org.junit.Assert.assertEquals


/**
 * Simulates an LSP client for test in a project defined by `sources`.
 *
 * @param sources The list of sources in the project
 */
class CodeTester(projects: List[Project]) {

  private val testServer = new TestServer(TestFile.testDir, projects)

  private val sources = for { project <- projects
                              source <- project.sources } yield (project, source)

  private val files =
    for { project <- projects
          (source, id) <- project.sources.zipWithIndex } yield source match {
      case src @ TastyWithPositions(text, _) => testServer.openCode(text, project, src.sourceName(id), openInIDE = false)
      case other => testServer.openCode(other.text, project, other.sourceName(id), openInIDE = true)
    }

  private val positions: PositionContext = getPositions(files)

  /** Check that the last diagnostics that have been published so far by the server
   *  for a given file match `expected`.
   *
   * @param marker   The marker defining the source file from which to query.
   * @param expected The expected diagnostics to be found
   */
  def diagnostics(marker: CodeMarker,
      expected: (CodeRange, String, DiagnosticSeverity, Option[ErrorMessageID])*): this.type = {
    implicit val posCtx = positions

    def toDiagnostic(range: CodeRange, message: String, severity: DiagnosticSeverity,
        errorCode: Option[ErrorMessageID]): Diagnostic = {
      new Diagnostic(range.toRange, message, severity, /*source =*/"",
        errorCode.map(_.errorNumber.toString).orNull)
    }

    val expectedParams = marker.toPublishDiagnosticsParams(expected.toList.map(toDiagnostic))
    // Find the latest published diagnostics for the current source file
    val actualParams = testServer.client.diagnostics.get.reverse.find(_.getUri == marker.uri)
      .getOrElse(throw new Exception(s"No published diagnostics for ${marker.uri}"))
    assertEquals(expectedParams, actualParams)

    this
  }

  /**
   * Perform a hover over `range`, verifies that result matches `expected`.
   *
   * @param range    The range over which to hover.
   * @param expected The expected result.
   *
   * @see dotty.tools.languageserver.util.actions.CodeHover
   */
  def hover(range: CodeRange, expected: Option[String]): this.type =
    doAction(new CodeHover(range, expected))

  /**
   * Perform a jump to definition over `range`, verifies that the results are `expected`.
   *
   * @param range    The range of positions from which run `jump to definition`.
   * @param expected The expected positions to jump to.
   *
   * @see dotty.tools.languageserver.util.actions.CodeDefinition
   */
  def definition(range: CodeRange, expected: Seq[CodeRange]): this.type =
    doAction(new CodeDefinition(range, expected))

  /**
   * Perform a highlight over `range`, verifies that the ranges and kinds of symbols match
   * `expected`.
   *
   * @param range    The range of positions to highlight.
   * @param expected The expected ranges and the kind of symbols that should be highlighted.
   *
   * @see dotty.tools.languageserver.util.actions.CodeDefinition
   */
  def highlight(range: CodeRange, expected: (CodeRange, DocumentHighlightKind)*): this.type =
    doAction(new CodeDocumentHighlight(range, expected))

  /**
   * Finds all the references to the symbol in `range`, verifies that the results match `expected`.
   *
   * @param range    The range of positions from which search for references.
   * @param expected The expected positions of the references
   * @param withDecl When set, include the declaration of the symbol under `range` in the results.
   *
   * @see dotty.tools.languageserver.util.actions.CodeReferences
   */
  def references(range: CodeRange, expected: List[CodeRange], withDecl: Boolean = false): this.type =
    doAction(new CodeReferences(range, expected, withDecl))

  /**
   * Requests completions at position `marker` with default value of `m1`, verifies that the results are empty
   * `expected`.
   *
   * @param marker   The position from which to ask for completions.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def noCompletions(marker: CodeMarker = m1): this.type =
    completion(marker, Set.empty)

  /**
   * Requests completion at the position defined by `marker`, verifies that the results match
   * `expected`.
   *
   * @param marker   The position from which to ask for completions.
   * @param expected The expected completion results.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def completion(marker: CodeMarker, expected: Set[(String, CompletionItemKind, String)]): this.type =
    completion(marker, results => assertEquals(expected, CodeCompletion.simplifyResults(results)))

  /**
   * Requests completion at default position defined by `marker`, verifies that the results match `expected`.
   *
   * @param marker   The position from which to ask for completions.
   * @param expected The expected completion results.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def completion(marker: CodeMarker, expected: (String, CompletionItemKind, String) *): this.type =
    completion(marker, expected.toSet)

  /**
   * Requests completion at the position defined by `marker`, and pass the results to
   * `checkResults`.
   *
   * @param marker       The position from which to ask for completions.
   * @param checkResults A function that verifies that the results of completion are correct.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def completion(marker: CodeMarker, checkResults: Set[CompletionItem] => Unit): this.type =
    doAction(new CodeCompletion(marker, checkResults))

  /**
   * Requests completion at default position defined by `m1`, verifies that the results match `expected`.
   *
   * @param expected The expected completion results.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def completion(expected: (String, CompletionItemKind, String) *): this.type =
    completion(m1, expected.toSet)

  /**
   * Requests completion at default position defined by `m1`, and pass the results to
   * `checkResults`.
   *
   * @param checkResults A function that verifies that the results of completion are correct.
   *
   * @see dotty.tools.languageserver.util.actions.CodeCompletion
   */
  def completion(checkResults: Set[CompletionItem] => Unit): this.type =
    doAction(new CodeCompletion(m1, checkResults))

  /**
   * Performs a workspace-wide renaming of the symbol under `marker`, verifies that the positions to
   * update match `expected`.
   *
   * @param marker         The position from which to ask for renaming.
   * @param newName        The new name to give to the symbol.
   * @param expected       The expected positions to change.
   * @param withOverridden If `None`, do not expect the server to ask whether to include overridden
   *                       symbol. Otherwise, wait for this question from the server and include
   *                       overridden symbols if this is true.
   *
   * @see dotty.tools.languageserver.util.actions.CodeRename
   */
  def rename(marker: CodeMarker,
             newName: String,
             expected: Set[CodeRange],
             withOverridden: Option[Boolean] = None): this.type =
    doAction(new CodeRename(marker, newName, expected, withOverridden)) // TODO apply changes to the sources and positions

  /**
   * Queries for all the symbols referenced in the source file in `marker`, verifies that they match
   * `expected`.
   *
   * @param marker   The marker defining the source file from which to query.
   * @param expected The expected symbols to be found.
   *
   * @see dotty.tools.languageserver.util.actions.CodeDocumentSymbol
   */
  def documentSymbol(marker: CodeMarker, expected: SymInfo*): this.type =
    doAction(new CodeDocumentSymbol(marker, expected))

  /**
   * Queries the whole workspace for symbols matching `query`, verifies that the results match
   * `expected`.
   *
   * @param query    The query used to find symbols.
   * @param expected The expected symbols to be found.
   *
   * @see dotty.tools.languageserver.util.actions.CodeSymbol
   */
  def symbol(query: String, symbols: SymInfo*): this.type =
    doAction(new CodeSymbol(query, symbols))

  /**
   * Triggers running the worksheet specified by `marker`, verifies that the results of
   * the run matches `expected`.
   *
   * @param marker   A marker a identifies the worksheet to evaluate.
   * @param expected The expected output.
   *
   * @see dotty.tools.languageserver.util.actions.WorksheetRun
   */
  def run(marker: CodeMarker, expected: (CodeRange, String)*): this.type =
    doAction(new WorksheetRun(marker, expected, strict = true))

  /**
   * Triggers running the worksheet specified by `marker`, verifies that each line of output
   * starts with `expected`.
   *
   * @param marker   A marker a identifies the worksheet to evaluate.
   * @param expected The expected starts of output.
   *
   * @see dotty.tools.languageserver.util.actions.WorksheetRun
   */
  def runNonStrict(marker: CodeMarker, expected: (CodeRange, String)*): this.type =
    doAction(new WorksheetRun(marker, expected, strict = false))

  /**
   * Triggers running the worksheet specified by `marker`, then verifies that execution can be
   * cancelled after `afterMs` milliseconds.
   *
   * @param marker   A marker that identifier the worksheet to evaluate.
   * @param afterMs  The delay in milliseconds before cancelling execution.
   *
   * @see dotty.tools.languageserver.util.actions.WorksheetCancel
   */
  def cancelRun(marker: CodeMarker, afterMs: Long): this.type =
    doAction(new WorksheetCancel(marker, afterMs))

  /**
   * Find implementations of the symbol in `range`, compares that the results match `expected.
   *
   * @param range    The range of position over which to run `textDocument/implementation`.
   * @param expected The expected result.
   */
  def implementation(range: CodeRange, expected: List[CodeRange]): this.type =
    doAction(new Implementation(range, expected))

  /**
   * Requests `textDocument/signatureHelp` from the language server at the specified `marker`,
   * verifies that the results match the expectations.
   *
   * @param marker          A maker that specifies the cursor position.
   * @param expected        The expected list of signature returned by the server.
   * @param activeSignature The expected active signature.
   * @param activeParam     The expected active parameter.
   *
   * @see dotty.tools.languageserver.util.actions.SignatureHelp
   */
  def signatureHelp(marker: CodeMarker,
                    expected: List[Signature],
                    activeSignature: Option[Int],
                    activeParam: Int): this.type =
    doAction(new SignatureHelp(marker, expected, activeSignature, activeParam))

  private def doAction(action: Action): this.type = {
    try {
      action.execute()(using testServer, testServer.client, positions)
    } catch {
      case ex: AssertionError =>
        val sourcesStr =
          sources.zip(files).map {
            case ((project, source), file) =>
              s"""// ${file.file} in project ${project.name}
                 |${source.text}""".stripMargin
          }.mkString(System.lineSeparator)

        val msg =
          s"""
            |
            |$sourcesStr
            |
            |while executing action: ${action.show(using positions)}
            |
          """.stripMargin
        val assertionError = new AssertionError(msg + ex.getMessage)
        assertionError.setStackTrace(ex.getStackTrace)
        throw assertionError
    }
    this
  }

  private def getPositions(files: List[TestFile]): PositionContext = {
    val posSeq = {
      for {
        ((_, code), file) <- sources.zip(files)
        (position, line, char) <- code.positions
      } yield position -> (file, line, char)
    }
    val posMap = posSeq.toMap
    assert(posSeq.size == posMap.size,
      "Each CodeMarker instance can only appear once in the code: " + posSeq.map(x => (x._1, x._2._2, x._2._3)))
    new PositionContext(posMap)
  }
}
