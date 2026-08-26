package dotty.tools.scaladoc
package snippets

import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

import scala.collection.mutable.ListBuffer

object SnippetExpectations:
  // Mirrors the compiler test suite (ParallelTesting.scala): space after `//` is optional.
  // Only `// error` and `// warn` are supported; `// anypos-*` is rejected.
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

  private def linePosition(sourceFile: SourceFile, sourceLine: Option[Int], relativeLine: Int): Option[Position] =
    sourceLine
      .flatMap(sourceFile.lineToOffsetOpt)
      .map(offset => Position(SourcePosition(sourceFile, Span(offset, offset)), relativeLine))

  case class ExpectedDiagnostic(
    level: MessageLevel,
    sourceLine: Option[Int],
    relativeLine: Int
  ):
    def position(sourceFile: SourceFile): Option[Position] =
      linePosition(sourceFile, sourceLine, relativeLine)

    def description: String =
      s"${level.text.toLowerCase} on line ${sourceLine.fold(relativeLine + 1)(_ + 1)}"

    // Matching is line-based only; column position is not checked.
    // Diagnostic message content is intentionally ignored.
    def matches(observed: ObservedDiagnostic): Boolean =
      observed.message.level == level && sourceLine == observed.sourceLine

  case class Parsed(
    expectations: Seq[ExpectedDiagnostic],
    parserErrors: Seq[SnippetCompilerMessage]
  ):
    def hasExpectations: Boolean = expectations.nonEmpty
    def expectedErrors: Int = expectations.count(_.level == MessageLevel.Error)

  case class ObservedDiagnostic(
    message: SnippetCompilerMessage,
    renderPosition: Option[Position],
    sourceLine: Option[Int]
  )

  /** Validates `observed` diagnostics of the given `level` against `expectations`.
   *  Returns error messages describing any mismatches, or `Nil` if everything matches. */
  private def validateLevel(
    level: MessageLevel,
    expectations: Seq[ExpectedDiagnostic],
    observed: Seq[ObservedDiagnostic],
    sourceFile: SourceFile
  ): Seq[SnippetCompilerMessage] =
    val levelName = level.text.toLowerCase
    if expectations.isEmpty then
      if observed.isEmpty then Nil
      else
        val ann = if level == MessageLevel.Warning then "warn" else "error"
        val summary = SnippetCompilerMessage(None,
          s"No expected ${levelName}s marked in snippet -- use // $ann\nactual $levelName count: ${observed.size}",
          MessageLevel.Error)
        summary +: observed.map(o =>
          SnippetCompilerMessage(o.message.position, s"Unexpected ${describeObserved(o)}", MessageLevel.Error))
    else
      val matched = Array.fill(observed.size)(false)
      val errors = ListBuffer.empty[SnippetCompilerMessage]

      for expectation <- expectations.sortBy(_.relativeLine) do
        observed.indices.find(i => !matched(i) && expectation.matches(observed(i))) match
          case Some(i) => matched(i) = true
          case None =>
            errors += SnippetCompilerMessage(
              expectation.position(sourceFile), s"Unfulfilled expectation: ${expectation.description}", MessageLevel.Error)

      for i <- observed.indices if !matched(i) do
        errors += SnippetCompilerMessage(
          observed(i).message.position, s"Unexpected ${describeObserved(observed(i))}", MessageLevel.Error)

      if errors.isEmpty then Nil
      else
        val summary =
          if expectations.size != observed.size then
            s"Wrong number of ${levelName}s encountered when compiling snippet\nexpected: ${expectations.size}, actual: ${observed.size}"
          else
            s"${level.text}s found on incorrect row numbers when compiling snippet"
        SnippetCompilerMessage(None, summary, MessageLevel.Error) +: errors.toSeq

  private def describeObserved(o: ObservedDiagnostic): String =
    val name = o.message.level.text.toLowerCase
    o.sourceLine match
      case Some(line) => s"$name on line ${line + 1}"
      case None => s"$name at an unknown position"

  /** Scans `snippet` for inline diagnostic annotations (`// error`, `// warn`).
   *  Returns parsed expectations together with any parse-level errors. */
  def parse(snippet: SnippetSource, sourceFile: SourceFile): Parsed =
    val expectations = ListBuffer.empty[ExpectedDiagnostic]
    val parserErrors = ListBuffer.empty[SnippetCompilerMessage]

    for ((line, sourceLine), relativeLine) <- snippet.snippet.linesIterator.zip(snippet.sourceLines).zipWithIndex do
      for m <- annotation.findAllMatchIn(line) do
        val isAnypos = m.group(1) != null
        val level = if m.group(2) == "warn" then MessageLevel.Warning else MessageLevel.Error
        if isAnypos then
          val ann = if level == MessageLevel.Warning then "warn" else "error"
          parserErrors += SnippetCompilerMessage(
            linePosition(sourceFile, sourceLine, relativeLine),
            s"Unsupported snippet diagnostic annotation `// anypos-$ann`; use `// $ann`",
            MessageLevel.Error)
        else
          expectations += ExpectedDiagnostic(level, sourceLine, relativeLine)

    Parsed(expectations.toList, parserErrors.toList)

  /** Converts raw compiler diagnostics into [[ObservedDiagnostic]]s by mapping
   *  positions back from the synthetic wrapper to the original snippet source. */
  def observe(
    diagnostics: Seq[Diagnostic],
    wrappedSnippet: WrappedSnippet,
    sourceFile: SourceFile
  ): Seq[ObservedDiagnostic] =
    diagnostics.toSeq.map: diagnostic =>
      val msg = try diagnostic.message catch case _: Exception => ""
      // Relies on MessageLevel ordinals matching dotty.tools.dotc.interfaces.Diagnostic
      // integer constants: INFO=0, WARNING=1, ERROR=2.
      val level = MessageLevel.fromOrdinal(diagnostic.level)
      val rawPos = adjustAtEOF(diagnostic.pos.nonInlined)
      val mappedPos =
        if rawPos.exists then wrappedSnippet.sourcePosition(rawPos, sourceFile)
        else None
      val renderPos =
        if rawPos.exists then wrappedSnippet.sourceSpanPosition(rawPos, sourceFile)
        else None
      ObservedDiagnostic(
        SnippetCompilerMessage(mappedPos, msg, level),
        renderPos,
        mappedPos.map(_.srcPos.line)
      )

  /** Matches observed diagnostics against parsed expectations.
   *  Returns error messages for any mismatches, or `Nil` when all match. */
  def validate(
    parsed: Parsed,
    observed: Seq[ObservedDiagnostic],
    sourceFile: SourceFile
  ): Seq[SnippetCompilerMessage] =
    val errorExpectations = parsed.expectations.filter(_.level == MessageLevel.Error)
    val warningExpectations = parsed.expectations.filter(_.level == MessageLevel.Warning)
    val errorDiagnostics = observed.filter(_.message.level == MessageLevel.Error)
    val warningDiagnostics = observed.filter(_.message.level == MessageLevel.Warning)

    parsed.parserErrors
      ++ validateLevel(MessageLevel.Error, errorExpectations, errorDiagnostics, sourceFile)
      ++ validateLevel(MessageLevel.Warning, warningExpectations, warningDiagnostics,
           sourceFile)
