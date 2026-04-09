package dotty.tools.scaladoc
package snippets

import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable.ListBuffer
import scala.util.Try

object SnippetExpectations:
  // Mirrors the compiler test suite (ParallelTesting.scala): space after `//` is optional.
  // Only `// error` and `// warn` are supported; `// anypos-*` is rejected, `// nopos-*`
  // is not recognised (position-less diagnostics don't arise in self-contained snippets).
  private val annotation =
    raw"""// *(anypos-)?(error|warn)\b""".r

  private def adjustAtEOF(pos: SourcePosition): SourcePosition =
    if pos.span.isSynthetic
      && pos.span.isZeroExtent
      && pos.span.exists
      && pos.source.exists
      && pos.span.start == pos.source.length
      && pos.span.start > 0
      && pos.source(pos.span.start - 1) == '\n'
    then pos.withSpan(pos.span.shift(-1))
    else pos

  case class ExpectedDiagnostic(
    level: MessageLevel,
    sourceLine: Option[Int],
    relativeLine: Int
  ):
    def position(sourceFile: SourceFile): Option[Position] =
      sourceLine
        .flatMap(sourceFile.lineToOffsetOpt)
        .map(offset => Position(SourcePosition(sourceFile, Span(offset, offset)), relativeLine))

    def description: String =
      s"${level.text.toLowerCase} on line ${sourceLine.fold(relativeLine + 1)(_ + 1)}"

    // Matching is line-based only; column position is not checked. Multiple
    // diagnostics on the same line are matched in declaration order.
    // Diagnostic message content is intentionally ignored: annotations like
    // `// error: some message` match any error on that line regardless of text.
    def matches(observed: ObservedDiagnostic): Boolean =
      observed.message.level == level && sourceLine == observed.sourceLine

  case class Parsed(
    expectations: Seq[ExpectedDiagnostic],
    parserErrors: Seq[SnippetCompilerMessage]
  ):
    def hasExpectations: Boolean = expectations.nonEmpty
    def expectedErrors: Int = expectations.count(_.level == MessageLevel.Error)

  case class ObservedDiagnostic(
    diagnostic: Diagnostic,
    message: SnippetCompilerMessage,
    renderPosition: Option[Position],
    sourceLine: Option[Int]
  )

  private def levelName(level: MessageLevel): String = level match
    case MessageLevel.Error => "error"
    case MessageLevel.Warning => "warning"
    case _ => level.text.toLowerCase

  private def levelNamePlural(level: MessageLevel): String = s"${levelName(level)}s"

  private def levelTitlePlural(level: MessageLevel): String = s"${level.text}s"

  private def annotationName(level: MessageLevel): String = level match
    case MessageLevel.Warning => "warn"
    case _ => "error"

  private def sourceLinePosition(
    sourceFile: SourceFile,
    sourceLine: Option[Int],
    relativeLine: Int
  ): Option[Position] =
    sourceLine
      .flatMap(sourceFile.lineToOffsetOpt)
      .map(offset => Position(SourcePosition(sourceFile, Span(offset, offset)), relativeLine))

  private def unexpectedDescription(observed: ObservedDiagnostic): String =
    val levelText = levelName(observed.message.level)
    observed.sourceLine match
      case Some(line) => s"$levelText on line ${line + 1}"
      case None => s"$levelText at an unknown position"

  private def unsupportedAnnotationMessage(level: MessageLevel): String =
    val ann = annotationName(level)
    s"Unsupported snippet diagnostic annotation `// anypos-$ann`; use `// $ann`"

  private def missingExpectationSummary(level: MessageLevel, actualCount: Int): String = level match
    case MessageLevel.Error =>
      s"""|No expected errors marked in snippet -- use // error
          |actual error count: $actualCount""".stripMargin
    case MessageLevel.Warning =>
      s"""|No expected warnings marked in snippet -- use // warn
          |actual warning count: $actualCount""".stripMargin
    case _ =>
      s"No expected ${levelNamePlural(level)} marked in snippet"

  private def validateLevel(
    level: MessageLevel,
    expectations: Seq[ExpectedDiagnostic],
    observed: Seq[ObservedDiagnostic],
    sourceFile: SourceFile
  ): Seq[SnippetCompilerMessage] =
    if expectations.isEmpty then
      if observed.isEmpty then Nil
      else
        val summary = SnippetCompilerMessage(None, missingExpectationSummary(level, observed.size), MessageLevel.Error)
        val unexpected = observed.map(observed =>
          SnippetCompilerMessage(
            observed.message.position,
            s"Unexpected ${unexpectedDescription(observed)}",
            MessageLevel.Error
          )
        )
        summary +: unexpected
    else
      val matched = Array.fill(observed.size)(false)
      val unfulfilled = ListBuffer.empty[(Option[Position], String)]
      val unexpected = ListBuffer.empty[(Option[Position], String)]

      for expectation <- expectations.sortBy(_.relativeLine) do
        val matchIdx = observed.indices.find(idx => !matched(idx) && expectation.matches(observed(idx)))
        matchIdx match
          case Some(idx) =>
            matched(idx) = true
          case None =>
            unfulfilled += ((expectation.position(sourceFile), expectation.description))

      for idx <- observed.indices if !matched(idx) do
        val diagnostic = observed(idx)
        unexpected += ((diagnostic.message.position, unexpectedDescription(diagnostic)))

      if unfulfilled.isEmpty && unexpected.isEmpty then Nil
      else
        val summary =
          if expectations.size != observed.size then
            s"""|Wrong number of ${levelNamePlural(level)} encountered when compiling snippet
                |expected: ${expectations.size}, actual: ${observed.size}""".stripMargin
          else
            s"${levelTitlePlural(level)} found on incorrect row numbers when compiling snippet"
        val summaryMessage = SnippetCompilerMessage(None, summary, MessageLevel.Error)
        val mismatchMessages =
          unfulfilled.map: (position, description) =>
            SnippetCompilerMessage(position, s"Unfulfilled expectation: $description", MessageLevel.Error)
          ++ unexpected.map: (position, description) =>
            SnippetCompilerMessage(position, s"Unexpected $description", MessageLevel.Error)
        summaryMessage +: mismatchMessages.toSeq

  /** Scans `snippet` for inline diagnostic annotations (`// error`, `// warn`) and
   *  returns them as a [[Parsed]] value together with any parse-level errors
   *  (e.g. unsupported `// anypos-*` annotations).
   */
  def parse(snippet: SnippetSource, sourceFile: SourceFile): Parsed =
    val expectations = ListBuffer.empty[ExpectedDiagnostic]
    val parserErrors = ListBuffer.empty[SnippetCompilerMessage]

    for ((line, sourceLine), relativeLine) <- snippet.snippet.linesIterator.zip(snippet.sourceLines).zipWithIndex do
      val annotations = annotation.findAllMatchIn(line).toList
      for m <- annotations do
        val isAnypos = Option(m.group(1)).isDefined
        val level = m.group(2) match
          case "warn" => MessageLevel.Warning
          case _ => MessageLevel.Error
        if isAnypos then
          parserErrors += SnippetCompilerMessage(
            sourceLinePosition(sourceFile, sourceLine, relativeLine),
            unsupportedAnnotationMessage(level),
            MessageLevel.Error
          )
        else
          expectations += ExpectedDiagnostic(level, sourceLine, relativeLine)

    Parsed(expectations.toList, parserErrors.toList)

  /** Converts raw compiler `diagnostics` into [[ObservedDiagnostic]] values by
   *  mapping their positions back from the synthetic wrapper to the original
   *  snippet source via `wrappedSnippet`.
   */
  def observe(
    diagnostics: Seq[Diagnostic],
    wrappedSnippet: WrappedSnippet,
    sourceFile: SourceFile
  ): Seq[ObservedDiagnostic] =
    diagnostics.toSeq.map: diagnostic =>
      val msg =
        Try(diagnostic.message) match
          case scala.util.Success(message) => message
          case scala.util.Failure(ex) => ex.getMessage
      // Relies on MessageLevel ordinals matching dotty.tools.dotc.interfaces.Diagnostic
      // integer constants: INFO=0, WARNING=1, ERROR=2. Keep MessageLevel case order in sync.
      val level = MessageLevel.fromOrdinal(diagnostic.level)
      val rawPos = adjustAtEOF(diagnostic.pos.nonInlined)
      val mappedPos =
        if rawPos.exists then wrappedSnippet.sourcePosition(rawPos, sourceFile)
        else None
      val renderPos =
        if rawPos.exists then wrappedSnippet.sourceSpanPosition(rawPos, sourceFile)
        else None
      ObservedDiagnostic(
        diagnostic,
        SnippetCompilerMessage(mappedPos, if msg == null then "" else msg, level),
        renderPos,
        mappedPos.map(_.srcPos.line)
      )

  /** Matches `observed` diagnostics against `parsed` expectations and returns
   *  [[SnippetCompilerMessage]] values describing any mismatches (unexpected
   *  diagnostics, unfulfilled expectations, wrong counts, wrong lines).
   *  Returns an empty sequence when all expectations are satisfied.
   */
  def validate(
    parsed: Parsed,
    observed: Seq[ObservedDiagnostic],
    sourceFile: SourceFile
  ): Seq[SnippetCompilerMessage] =
    val errorExpectations = parsed.expectations.filter(_.level == MessageLevel.Error)
    val warningExpectations = parsed.expectations.filter(_.level == MessageLevel.Warning)
    val errorDiagnostics = observed.filter(_.message.level == MessageLevel.Error)
    val warningDiagnostics = observed.filter(_.message.level == MessageLevel.Warning)

    parsed.parserErrors ++
      validateLevel(MessageLevel.Error, errorExpectations, errorDiagnostics, sourceFile) ++
      validateLevel(MessageLevel.Warning, warningExpectations, warningDiagnostics, sourceFile)
