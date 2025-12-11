package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import java.lang.System.{lineSeparator => EOL}

import core.Contexts.*
import core.Decorators.*
import io.AbstractFile
import printing.Highlighting.{Blue, Red, Yellow}
import printing.SyntaxHighlighting
import Diagnostic.*
import util.{SourcePosition, NoSourcePosition}
import util.Chars.{ LF, CR, FF, SU }
import scala.annotation.switch

import scala.collection.mutable.StringBuilder

trait MessageRendering {
  import Highlight.*
  import Offsets.*

  /** The maximal number of lines of code that are shown in a message after the
   *  `^` and error message.
   */
  private inline val maxRenderedLinesAfterPoint = 3

  /** Remove ANSI coloring from `str`, useful for getting real length of
    * strings
    *
    * @return string stripped of ANSI escape codes
    */
  def stripColor(str: String): String =
    str.replaceAll("\u001b\\[.*?m", "")

  /** Get the sourcelines before and after the position, as well as the offset
    * for rendering line numbers
    *
    * @return (lines before error, lines after error, line numbers offset)
    */
  private def sourceLines(pos: SourcePosition)(using Context, Level, Offset): (List[String], List[String], Int) = {
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

    def compress(offsetsAndLines: List[(Int, String)]): List[(Int, String)] =
      if offsetsAndLines.isEmpty then offsetsAndLines
      else
        val compressedLines =
          if offsetsAndLines.length > maxRenderedLinesAfterPoint then
            offsetsAndLines.take(maxRenderedLinesAfterPoint - 2)
            ++ List(
                (offsetsAndLines(maxRenderedLinesAfterPoint - 2)._1, "..."),
                offsetsAndLines.last)
          else offsetsAndLines
        compressedLines

    (
      before.zip(lines).map(render),
      compress(after.zip(lines.drop(before.length))).map(render),
      maxLen
    )
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

  /** The horizontal line with the given offset
   *
   *  ```
   *    |
   *  ```
   */
  private def offsetBox(using Context, Level, Offset): String =
    val prefix = " " * (offset - 1)
    hl(s"$prefix|")

  /** The end of a box section
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
    * @return aligned error message
    */
  private def errorMsg(pos: SourcePosition, msg: String, addLine: Boolean)(using Context, Level, Offset): String = {
    val padding = msg.linesIterator.foldLeft(pos.startColumnPadding) { (pad, line) =>
      val lineLength = stripColor(line).length
      val maxPad = math.max(0, ctx.settings.pageWidth.value - offset - lineLength) - offset

      if (maxPad < pad.length) " " * maxPad
      else pad
    }

    val msgStr = msg.linesIterator
      .map { line => offsetBox + (if line.isEmpty then "" else padding + line) }
      .mkString(EOL)
    if addLine then msgStr ++ s"${EOL}$offsetBox" else msgStr
  }

  // file.path or munge it to normalize for testing
  protected def renderPath(file: AbstractFile): String = file.path

  /** The source file path, line and column numbers from the given SourcePosition */
  protected def posFileStr(pos: SourcePosition): String =
    val path = renderPath(pos.source.file)
    if pos.exists then s"$path:${pos.line + 1}:${pos.column}" else path

  /** The separator between errors containing the source file and error type
    *
    * @return separator containing error location and kind
    */
  private def posStr(
    pos: SourcePosition,
    message: Message,
    diagnosticString: String
  )(using Context, Level, Offset): String =
    assert(
      message.errorId.isActive,
      """|Attempting to use an ErrorMessageID that is marked as inactive.
         |The ID either needs to be marked as active or you need to use another.""".stripMargin
    )
    if (pos.source != NoSourcePosition.source) then
      hl({
        val realPos = pos.nonInlined
        val fileAndPos = posFileStr(realPos)
        val errId =
          if (message.errorId != ErrorMessageID.NoExplanationID) then
            val errorNumber = message.errorId.errorNumber
            s"[E${"0" * (3 - errorNumber.toString.length) + errorNumber}] "
          else ""
        val kind =
          if (message.kind == MessageKind.NoKind) then diagnosticString
          else s"${message.kind.message} $diagnosticString"
        val title =
          if fileAndPos.isEmpty then s"$errId$kind:" // this happens in dotty.tools.repl.ScriptedTests // TODO add name of source or remove `:` (and update test files)
          else s"$errId$kind: $fileAndPos"
        boxTitle(title)
      })
    else ""
  end posStr

  /** Explanation rendered under "Explanation" header */
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

  private def appendFilterHelp(dia: Diagnostic, sb: StringBuilder)(using Context, Level, Offset): Unit =
    extension (sb: StringBuilder) def nl: sb.type = sb.append(EOL).append(offsetBox)
    import dia.msg
    val hasId = msg.errorId.errorNumber >= 0
    val (category, origin) = dia match
      case _: UncheckedWarning   => ("unchecked", "")
      case w: DeprecationWarning => ("deprecation", w.origin)
      case _: FeatureWarning     => ("feature", "")
      case _: ConfigurationWarning => ("configuration", "")
      case _                     => ("", "")
    var entitled = false
    def addHelp(what: String)(value: String): Unit =
      if !entitled then
        sb.nl.append("Matching filters for @nowarn or -Wconf:")
        entitled = true
      sb.nl.append("  - ").append(what).append(value)
    if hasId then
      addHelp("id=E")(msg.errorId.errorNumber.toString)
      addHelp("name=")(msg.errorId.productPrefix.stripSuffix("ID"))
    if category.nonEmpty then
      addHelp("cat=")(category)
    if origin.nonEmpty then
      addHelp("origin=")(origin)

  /** The whole message rendered from `dia.msg`.
   *
   *  For a position in an inline expansion, choose `pos1`
   *  which is the most specific position in the call written
   *  by the user. For a diagnostic at EOF, where the last char
   *  of source text is a newline, adjust the position to render
   *  before the newline, at the end of the last line of text.
   *
   *  The rendering begins with a label and position (`posString`).
   *  Then `sourceLines` with embedded caret `positionMarker`
   *  and rendered message.
   *
   *  Then an `Inline stack trace` showing context for inlined code.
   *  Inlined positions are taken which don't contain `pos1`.
   *  (That should probably be positions not contained by outermost.)
   *  Note that position equality includes `outer` position;
   *  usually we intend to test `contains` or `coincidesWith`.
   *
   */
  def messageAndPos(dia: Diagnostic)(using Context): String =
    // adjust a pos at EOF if preceded by newline
    def adjust(pos: SourcePosition): SourcePosition =
      if pos.span.isSynthetic
      && pos.span.isZeroExtent
      && pos.span.exists
      && pos.span.start == pos.source.length
      && pos.source(pos.span.start - 1) == '\n'
      then
        pos.withSpan(pos.span.shift(-1))
      else
        pos
    val msg = dia.msg
    val pos = dia.pos
    val pos1 = adjust(pos.nonInlined) // innermost pos contained by call.pos
    val outermost = pos.outermost // call.pos
    val inlineStack = pos.inlinePosStack.filterNot(outermost.contains(_))
    given Level = Level(dia.level)
    given Offset =
      val maxLineNumber =
        if pos.exists then (pos1 :: inlineStack).map(_.endLine).max + 1
        else 0
      Offset(maxLineNumber.toString.length + 2)
    val sb = StringBuilder()
    val posString = posStr(pos1, msg, diagnosticLevel(dia))
    if posString.nonEmpty then sb.append(posString).append(EOL)
    if pos.exists && pos1.exists && pos1.source.file.exists then
      val (srcBefore, srcAfter, offset) = sourceLines(pos1)
      val marker = positionMarker(pos1)
      val err = errorMsg(pos1, msg.message, srcAfter.nonEmpty)
      sb.append((srcBefore ::: marker :: err :: srcAfter).mkString(EOL))

      if inlineStack.nonEmpty then
        sb.append(EOL).append(newBox())
        sb.append(EOL).append(offsetBox).append(i"Inline stack trace")
        for inlinedPos <- inlineStack do
          sb.append(EOL).append(newBox(soft = true))
          sb.append(EOL).append(offsetBox).append(i"This location contains code that was inlined from $pos")
          if inlinedPos.source.file.exists then
            val (srcBefore, srcAfter, _) = sourceLines(inlinedPos)
            val marker = positionMarker(inlinedPos)
            sb.append(EOL).append((srcBefore ::: marker :: srcAfter).mkString(EOL))
        sb.append(EOL).append(endBox)
      end if
    else sb.append(msg.message)
    if dia.isVerbose then
      appendFilterHelp(dia, sb)

    if Diagnostic.shouldExplain(dia) then
      sb.append(EOL).append(newBox())
      sb.append(EOL).append(offsetBox).append(" Explanation (enabled by `-explain`)")
      sb.append(EOL).append(newBox(soft = true))
      dia.msg.explanation.split(raw"\R").foreach: line =>
        sb.append(EOL).append(offsetBox).append(if line.isEmpty then "" else " ").append(line)
      sb.append(EOL).append(endBox)
    else if dia.msg.canExplain then
      sb.append(EOL).append(offsetBox)
      sb.append(EOL).append(offsetBox).append(" longer explanation available when compiling with `-explain`")

    sb.toString
  end messageAndPos

  private def hl(str: String)(using Context, Level): String =
    summon[Level].value match
      case interfaces.Diagnostic.ERROR   => Red(str).show
      case interfaces.Diagnostic.WARNING => Yellow(str).show
      case interfaces.Diagnostic.INFO    => Blue(str).show

  private def diagnosticLevel(dia: Diagnostic): String =
    dia match {
      case dia: FeatureWarning => "Feature Warning"
      case dia: DeprecationWarning => "Deprecation Warning"
      case dia: UncheckedWarning => "Unchecked Warning"
      case dia: MigrationWarning => "Migration Warning"
      case _ => dia.level match // Diagnostic isn't sealed (e.g. created in the REPL) so provide a fallback
        case interfaces.Diagnostic.ERROR   => "Error"
        case interfaces.Diagnostic.WARNING => "Warning"
        case interfaces.Diagnostic.INFO    => "Info"
    }

}

private object Highlight {
  opaque type Level = Int
  extension (level: Level) def value: Int = level
  object Level:
    def apply(level: Int): Level = level
}

/** Size of the left offset added by the box
 *
 *  ```
 *  -- Error: ... ------------
 *  4 |  foo
 *    |  ^^^
 *  ^^^ // size of this offset
 *  ```
 */
private object Offsets {
  opaque type Offset = Int
  def offset(using o: Offset): Int = o
  object Offset:
    def apply(level: Int): Offset = level
}
