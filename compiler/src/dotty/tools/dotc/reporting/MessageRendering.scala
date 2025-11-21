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

    (
      before.zip(lines).map(render),
      after.zip(lines.drop(before.length)).map(render),
      maxLen
    )
  }

  /** Generate box containing the report title
   *
   *  ```
   *  -- Error: source.scala ---------------------
   *  ```
   */
  private def boxTitle(title: String, isSubtitle: Boolean = false)(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val line = "-" * (pageWidth - title.length - 4)
    val starter = if isSubtitle then ".." else "--"
    hl(s"$starter $title $line")

  /** The position markers aligned under the error
   *
   *  ```
   *    |         ^^^^^
   *  ```
   *  or for sub-diagnostics:
   *  ```
   *    |         -----
   *  ```
   *
   *  @param pos the source position to mark
   *  @param markerChar the character to use for marking ('^' for primary errors, '-' for notes)
   */
  private def positionMarker(pos: SourcePosition, markerChar: Char = '^')(using Context, Level, Offset): String = {
    val padding = pos.startColumnPadding
    val markers =
      if (pos.startLine == pos.endLine)
        markerChar.toString * math.max(1, pos.endColumn - pos.startColumn)
      else markerChar.toString
    hl(s"$offsetBox$padding$markers")
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
    diagnosticString: String,
    isSubdiag: Boolean = false
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
        boxTitle(title, isSubtitle = isSubdiag)
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

  /** Render a message using multi-span information from Message.parts. */
  def messageAndPosFromParts(dia: Diagnostic)(using Context): String =
    val msg = dia.msg
    val pos = dia.pos
    val pos1 = adjust(pos.nonInlined)
    val msgParts = msg.parts

    if msgParts.isEmpty then
      return msg.leading.getOrElse("") + (if msg.leading.isDefined then "\n" else "") + msg.message

    // Collect all positions from message parts
    val validParts = msgParts.filter(_.srcPos.exists)

    if validParts.isEmpty then
      return msg.leading.getOrElse("") + (if msg.leading.isDefined then "\n" else "") + msg.message

    // Check all positions are in the same source file
    val source = validParts.head.srcPos.source
    if !validParts.forall(_.srcPos.source == source) || !source.file.exists then
      // TODO: support rendering source positions across multiple files
      return msg.leading.getOrElse("") + (if msg.leading.isDefined then "\n" else "") + msg.message

    // Find the line range covering all positions
    val minLine = validParts.map(_.srcPos.startLine).min
    val maxLine = validParts.map(_.srcPos.endLine).max
    val maxLineNumber = maxLine + 1

    given Level = Level(dia.level)
    given Offset = Offset(maxLineNumber.toString.length + 2)

    val sb = StringBuilder()

    // Title using the primary position
    val posString = posStr(pos1, msg, diagnosticLevel(dia))
    if posString.nonEmpty then sb.append(posString).append(EOL)

    // Display leading text if present
    msg.leading.foreach { leadingText =>
      sb.append(leadingText)
      if !leadingText.endsWith(EOL) then sb.append(EOL)
    }

    // Render the unified code snippet
    val startOffset = source.lineToOffset(minLine)
    val endOffset = source.nextLine(source.lineToOffset(maxLine))
    val content = source.content.slice(startOffset, endOffset)
    val syntax =
      if (summon[Context].settings.color.value != "never" && !summon[Context].isJava)
        SyntaxHighlighting.highlight(new String(content)).toCharArray
      else content

    // Split syntax-highlighted content into lines
    def linesFrom(arr: Array[Char]): List[String] = {
      def pred(c: Char) = (c: @switch) match {
        case LF | CR | FF | SU => true
        case _ => false
      }
      val (line, rest0) = arr.span(!pred(_))
      val (_, rest) = rest0.span(pred)
      new String(line) :: { if (rest.isEmpty) Nil else linesFrom(rest) }
    }

    val lines = linesFrom(syntax)
    val lineNumberWidth = maxLineNumber.toString.length

    // Render each line with its markers and messages
    for (lineNum <- minLine to maxLine) do
      val lineIdx = lineNum - minLine
      if lineIdx < lines.length then
        val lineContent = lines(lineIdx)
        val lineNbr = (lineNum + 1).toString
        val linePrefix = String.format(s"%${lineNumberWidth}s |", lineNbr)
        val lnum = hl(" " * math.max(0, offset - linePrefix.length - 1) + linePrefix)
        sb.append(lnum).append(lineContent.stripLineEnd).append(EOL)

        // Find all positions that should show markers after this line
        val partsOnLine = validParts.filter(_.srcPos.startLine == lineNum)
          .sortBy(p => (p.srcPos.startColumn, !p.isPrimary))

        if partsOnLine.size == 1 then
          // Single marker on this line
          val part = partsOnLine.head
          val markerChar = if part.isPrimary then '^' else '-'
          val marker = positionMarker(part.srcPos, markerChar)
          val err = errorMsg(part.srcPos, part.text)
          sb.append(marker).append(EOL)
          sb.append(err).append(EOL)
        else if partsOnLine.size > 1 then
          // Multiple markers on same line
          val markerLine = StringBuilder()
          markerLine.append(offsetBox)

          var currentCol = 0
          for part <- partsOnLine do
            val markerChar = if part.isPrimary then '^' else '-'
            val targetCol = part.srcPos.startColumn
            val padding = " " * (targetCol - currentCol)
            markerLine.append(padding).append(markerChar)
            currentCol = targetCol + 1

          sb.append(markerLine).append(EOL)

          // Render messages from right to left with connector bars
          val sortedByColumn = partsOnLine.reverse // rightmost first
          for (part, idx) <- sortedByColumn.zipWithIndex do
            val remainingParts = sortedByColumn.drop(idx + 1) // parts still waiting for messages

            // Build connector line with vertical bars for remaining parts
            val connectorLine = StringBuilder()
            connectorLine.append(offsetBox)

            var col = 0
            // First, add vertical bars for all remaining (not-yet-shown) parts
            for p <- partsOnLine do
              if remainingParts.contains(p) then
                val targetCol = p.srcPos.startColumn
                val padding = " " * (targetCol - col)
                connectorLine.append(padding).append("|")
                col = targetCol + 1

            // Then add the message for the current part, aligned to its column
            val msgText = part.text
            val msgCol = part.srcPos.startColumn
            // If we've added bars, col is position after last bar; if not, col is 0
            // We want the message to start at msgCol, with at least one space separation
            val msgPadding = if col == 0 then " " * msgCol else " " * Math.max(1, msgCol - col)
            connectorLine.append(msgPadding).append(msgText)

            sb.append(connectorLine).append(EOL)

    // Add explanation if needed
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
  end messageAndPosFromParts

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
    val msg = dia.msg
    // Check if message provides its own multi-span structure
    if msg.leading.isDefined || msg.parts.nonEmpty then
      messageAndPosFromParts(dia)
    else
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
        val err = errorMsg(pos1, msg.message)
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
