package dotty.tools
package dotc
package reporting

import language.unsafeNulls

import java.lang.System.lineSeparator as EOL

import core.Contexts.{Context, ctx}
import core.Decorators.*
import io.AbstractFile
import printing.Highlighting.{Blue, Red, Yellow}
import printing.SyntaxHighlighting
import util.{SourcePosition, NoSourcePosition}
import util.Chars.{LF, CR, FF, SU}
import Diagnostic.*

import scala.annotation.switch
import scala.collection.mutable.StringBuilder

trait MessageRendering:
  import MessageRendering.*

  /** Remove ANSI coloring from `str`.
   *
   *  Useful for getting real length of highlighted strings.
   *
   *  @return string stripped of ANSI escape codes
   */
  def stripColor(str: String): String = Highlight.stripColor(str)

  // file.path or munge it to normalize for testing
  protected def renderPath(file: AbstractFile): String = file.path

  /** The source file path, line and column numbers from the given SourcePosition. */
  protected def posFileStr(pos: SourcePosition): String =
    val path = renderPath(pos.source.file)
    if pos.exists then s"$path:${pos.line + 1}:${pos.column}" else path

  /** Explanation rendered under "Explanation" header. */
  def explanation(m: Message)(using Context): String = {
    val sb = new StringBuilder(
      s"""|
          |${Blue("Explanation").show}
          |${Blue("===========").show}""".stripMargin
    )
    sb.append(EOL).append(m.explanation)
    if (!m.explanation.endsWith(EOL)) sb.append(EOL)
    sb.toString
  }

  /** The whole message rendered from `dia.msg`.
   *
   *  For a position in an inline expansion, choose `pos1`
   *  which is the most specific position in the call written by the user.
   *
   *  For a diagnostic at EOF, where the last char of source text is a newline,
   *  adjust the position to render before the newline, at the end of the last line of text.
   *
   *  The rendering begins with a `header` showing a label and position.
   *  Then `sourceLines` with embedded caret `positionMarker` and rendered message.
   *
   *  Then an `Inline stack trace` showing context for inlined code.
   *  Inlined positions are taken which are not contained by outermost.
   *  Note that position equality includes `outer` position;
   *  usually we intend to test `contains` or `coincidesWith`.
   *
   *  A verbose diagnostic adds help for how to silence it.
   *
   *  A diagnostic offering an `explanation` may add that text.
   */
  def messageAndPos(dia: Diagnostic)(using Context): String =
    import Renderer.{dia as _, *}
    import Boxing.Inset
    render(dia):
      header(posFileStr)
      if pos.exists && pos.source.file.exists then
        sourceLines()
        val inlineStack = dia.pos.inlinedPositions
        if inlineStack.nonEmpty then
          box("Inline stack trace"):
            for inlinedPos <- inlineStack do
              box(i"This location contains code that was inlined from ${dia.pos}"):
                if inlinedPos.source.file.exists then
                  sourceLines(inlinedPos)
      else message()
      if dia.isVerbose then
        filterHelp()
      locally:
        given Inset = Inset(1)
        if Diagnostic.shouldExplain(dia) then
          box("Explanation (enabled by `-explain`)"):
            box(""):
              dia.msg.explanation.split(raw"\R").foreach: line =>
                sb.nl(line)
        else if dia.msg.canExplain then
          sb.nl("").nl("longer explanation available when compiling with `-explain`")
object MessageRendering:
  extension (pos: SourcePosition)
    /** `inlinePosStack` filtering out positions contained by the `call` written by the user */
    def inlinedPositions =
      val outermost = pos.outermost // call.pos
      pos.inlinePosStack.filterNot(outermost.contains(_))
end MessageRendering

private object Highlight:
  opaque type Level = Int
  extension (level: Level) def value: Int = level
  object Level:
    def apply(level: Int): Level = level

  private val stripper = raw"\u001b\[.*?m".r

  /** Remove ANSI coloring from `str`, useful for getting real length of strings.
   *
   *  @return string stripped of ANSI escape codes
   */
  def stripColor(str: String): String = stripper.replaceAllIn(str, "")

  def hl(str: String)(using Context, Level): String =
    summon[Level].value match
    case interfaces.Diagnostic.ERROR   => Red(str).show
    case interfaces.Diagnostic.WARNING => Yellow(str).show
    case interfaces.Diagnostic.INFO    => Blue(str).show
end Highlight

/** Size of the left offset added by the box.
 *
 *  ```
 *  -- Error: ... ------------
 *  4 |  foo
 *    |  ^^^
 *  ^^^ // size of this offset
 *  ```
 */
private object Offsets:
  opaque type Offset = Int
  def offset(using o: Offset): Int = o
  object Offset:
    def apply(level: Int): Offset = level

private object Boxing:
  opaque type Nesting = Boolean
  val top: Nesting = false
  val nested: Nesting = true
  extension (nesting: Nesting) def isNested: Boolean = nesting

  opaque type Inset = String
  object Inset:
    def apply(n: Int): Inset = if n == 0 then "" else " " //" " * n
end Boxing

private object Renderer:
  import Highlight.*
  import Offsets.*
  import MessageRendering.*
  import Rendering.*
  import Boxing.*
  opaque type Rendering = StringBuilder
  object Rendering:
    extension (r: Rendering)
      def append(x: Any): r.type = { r.append(x); r }
      def nl(s: String)(using Context, Level, Offset, Inset): r.type =
        append(EOL).append(offsetBox)
        if !s.isEmpty then append(summon[Inset])
        append(s)
  opaque type DiagnosticPosition = SourcePosition
  object DiagnosticPosition:
    extension (pos: DiagnosticPosition)
      private def p = pos
      export p.{exists, source}

  def sb(using Rendering) = summon[Rendering]
  def dia(using Diagnostic) = summon[Diagnostic]
  def pos(using DiagnosticPosition) = summon[DiagnosticPosition]

  /** Entry point for rendering the diagnostic. */
  def render(dia: Diagnostic)
      (op: (Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering, Inset) ?=> Unit)
      (using Context): String =
    given Diagnostic = dia
    given DiagnosticPosition = adjust(dia.pos.nonInlined) // innermost pos contained by call.pos
    given Level = Level(dia.level)
    given Offset =
      val maxLineNumber =
        if pos.exists then (pos :: dia.pos.inlinedPositions).map(_.endLine).max + 1
        else 0
      Offset(maxLineNumber.toString.length + 2)
    given Rendering = StringBuilder()
    given Inset = Inset(0)
    op
    sb.toString

  /** Add the header line, which shows the source file and error type. */
  def header(posRenderer: SourcePosition => String)
      (using Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering): Unit =
    val message = dia.msg
    assert(
      message.errorId.isActive,
      """|Attempting to use an ErrorMessageID that is marked as inactive.
         |The ID either needs to be marked as active or you need to use another.""".stripMargin
    )
    if pos.source != NoSourcePosition.source then
      val fileAndPos = posRenderer(pos)
      val errId =
        if message.errorId != ErrorMessageID.NoExplanationID then
          val errorNumber = message.errorId.errorNumber
          s"[E${"0" * (3 - errorNumber.toString.length) + errorNumber}] "
        else ""
      val kind =
        if message.kind == MessageKind.NoKind then diagnosticLevel
        else s"${message.kind.message} $diagnosticLevel"
      val title =
        if fileAndPos.isEmpty then s"$errId$kind:"
          // this happens in dotty.tools.repl.ScriptedTests
          // please add name of source or remove `:` (and update test files)
        else s"$errId$kind: $fileAndPos"
      sb.append(hl(boxTitle(title))).append(EOL)

  /** Add source text at the diagnostic position and error message text. */
  def sourceLines()(using Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering): Unit =
    val (srcBefore, srcAfter, offset) = sourceTexts(pos)
    val marker = positionMarker(pos)
    val err = errorMsg(pos, dia.msg.message)
    sb.append((srcBefore ::: marker :: err :: srcAfter).mkString(EOL))
  /** Quote the source text at the provided position. Used for inlined positions. */
  def sourceLines(pos: SourcePosition)
      (using Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering): Unit =
    val (srcBefore, srcAfter, _) = sourceTexts(pos)
    srcBefore.foreach(s => sb.append(EOL).append(s))
    sb.append(EOL).append(positionMarker(pos))
    srcAfter.foreach(s => sb.append(EOL).append(s))
  /** Add a box. The `soft` flag makes the divider less severe. */
  def box(title: String)
      (op: (Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering, Nesting, Inset) ?=> Unit)
      (using Context, Level, Offset, Diagnostic, DiagnosticPosition, Rendering, Inset)
      (using nesting: Nesting = Boxing.top): Unit =
    sb.append(EOL).append(newBox(nesting.isNested))
    if !title.isEmpty then
      sb.nl(title)
    given Nesting = Boxing.nested
    op
    if !nesting.isNested then
      sb.append(EOL).append(endBox)
  /** Add just the message. */
  def message()(using Context, Diagnostic, Rendering): Unit =
    sb.append(dia.msg.message)

  // adjust a pos at EOF if preceded by newline
  private def adjust(pos: SourcePosition): SourcePosition =
    if pos.span.isSynthetic
    && pos.span.isZeroExtent
    && pos.span.exists
    && pos.span.start == pos.source.length
    && pos.source(pos.span.start - 1) == '\n'
    then
      pos.withSpan(pos.span.shift(-1))
    else
      pos

  private def diagnosticLevel(using dia: Diagnostic): String =
    dia match
    case dia: FeatureWarning     => "Feature Warning"
    case dia: DeprecationWarning => "Deprecation Warning"
    case dia: UncheckedWarning   => "Unchecked Warning"
    case dia: MigrationWarning   => "Migration Warning"
    case dia =>
      dia.level match // Diagnostic isn't sealed (e.g. created in the REPL) so provide a fallback
      case interfaces.Diagnostic.ERROR   => "Error"
      case interfaces.Diagnostic.WARNING => "Warning"
      case interfaces.Diagnostic.INFO    => "Info"

  /** Get the sourcelines before and after the position, as well as the offset
   *  for rendering line numbers
   *
   *  @return (lines before error, lines after error, line numbers offset)
   */
  private def sourceTexts(pos: SourcePosition)(using Context, Level, Offset): (List[String], List[String], Int) = {
    assert(pos.exists && pos.source.file.exists)
    var maxLen = Int.MinValue
    def render(offsetAndLine: (Int, String)): String = {
      val (offset1, line) = offsetAndLine
      val lineNbr = (pos.source.offsetToLine(offset1) + 1).toString
      val prefix = String.format(s"%${offset - 2}s |", lineNbr)
      maxLen = math.max(maxLen, prefix.length)
      val lnum = hl(" " * math.max(0, maxLen - prefix.length - 1) + prefix)
      lnum + line.stripLineEnd
    }

    def linesFrom(arr: Array[Char]): List[String] = {
      def pred(c: Char) = (c: @switch) match {
        case LF | CR | FF | SU => true
        case _ => false
      }
      val (line, rest0) = arr.span(!pred(_))
      val (_, rest) = rest0.span(pred)
      new String(line) :: { if (rest.isEmpty) Nil else linesFrom(rest) }
    }

    val syntax =
      if (ctx.settings.color.value != "never" && !ctx.isJava)
        SyntaxHighlighting.highlight(new String(pos.linesSlice)).toCharArray
      else pos.linesSlice
    val lines = linesFrom(syntax)
    val (before, after) = pos.beforeAndAfterPoint

    (
      before.zip(lines).map(render),
      after.zip(lines.drop(before.length)).map(render),
      maxLen
    )
  }

  /** The position markers aligned under the error
   *
   *  ```
   *    |         ^^^^^
   *  ```
   */
  private def positionMarker(pos: SourcePosition)(using Context, Level, Offset): String = {
    val padding = pos.startColumnPadding
    val carets =
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"
    hl(s"$offsetBox$padding$carets")
  }

  /** Generate box containing the report title
   *
   *  ```
   *  -- Error: source.scala ---------------------
   *  ```
   */
  private def boxTitle(title: String)(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val line = "-" * (pageWidth - title.length - 4)
    hl(s"-- $title $line")

  /** Indent to the given offset and add a vertical bar in a box.
   *
   *  ```
   *    |
   *  ```
   */
  private def offsetBox(using Context, Level, Offset): String =
    val prefix = " " * (offset - 1)
    hl(s"$prefix|")

  /** The start of a box section.
   *
   *  ```
   *    |---------------
   *  ```
   *  Or if there `soft` is true,
   *  ```
   *    |- - - - - - - -
   *  ```
   */
  private def newBox(soft: Boolean = false)(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val prefix = " " * (offset - 1)
    val lineWidth = (pageWidth - offset)
    val line = if soft then ("- " * ((lineWidth + 1) / 2)).trim else "-" * lineWidth
    hl(s"$prefix|$line")

  /** The end of a box section
   *
   *  ```
   *     ----------------
   *  ```
   */
  private def endBox(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val prefix = " " * (offset - 1)
    val line = "-" * (pageWidth - offset)
    hl(s"${prefix} $line")

  /** The error message (`msg`) aligned under `pos`
   *
   *  @return aligned error message
   */
  private def errorMsg(pos: SourcePosition, msg: String)(using Context, Level, Offset): String = {
    val padding = msg.linesIterator.foldLeft(pos.startColumnPadding) { (pad, line) =>
      val lineLength = stripColor(line).length
      val maxPad = math.max(0, ctx.settings.pageWidth.value - offset - lineLength) - offset

      if (maxPad < pad.length) " " * maxPad
      else pad
    }

    msg.linesIterator
      .map { line => offsetBox + (if line.isEmpty then "" else padding + line) }
      .mkString(EOL)
  }

  def filterHelp()(using Context, Level, Offset, Diagnostic, Rendering, Inset): Unit =
    val msg = dia.msg
    val hasId = msg.errorId.errorNumber >= 0
    val (category, origin) = dia match
      case _: UncheckedWarning   => ("unchecked", "")
      case w: DeprecationWarning => ("deprecation", w.origin)
      case _: FeatureWarning     => ("feature", "")
      case _                     => ("", "")
    var entitled = false
    def addHelp(what: String)(value: String): Unit =
      if !entitled then
        sb.nl("Matching filters for @nowarn or -Wconf:")
        entitled = true
      sb.nl("  - ").append(what).append(value)
    if hasId then
      addHelp("id=E")(msg.errorId.errorNumber.toString)
      addHelp("name=")(msg.errorId.productPrefix.stripSuffix("ID"))
    if category.nonEmpty then
      addHelp("cat=")(category)
    if origin.nonEmpty then
      addHelp("origin=")(origin)
end Renderer
