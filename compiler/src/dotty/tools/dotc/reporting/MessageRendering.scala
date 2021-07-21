package dotty.tools
package dotc
package reporting

import java.lang.System.{lineSeparator => EOL}

import core.Contexts._
import core.Decorators._
import printing.Highlighting.{Blue, Red, Yellow}
import printing.SyntaxHighlighting
import Diagnostic._
import util.{ SourcePosition, NoSourcePosition }
import util.Chars.{ LF, CR, FF, SU }
import scala.annotation.switch

import scala.collection.mutable

trait MessageRendering {

  /** Remove ANSI coloring from `str`, useful for getting real length of
    * strings
    *
    * @return string stripped of ANSI escape codes
    */
  def stripColor(str: String): String =
    str.replaceAll("\u001b\\[.*?m", "")

  /** When inlining a method call, if there's an error we'd like to get the
    * outer context and the `pos` at which the call was inlined.
    *
    * @return a list of strings with inline locations
    */
  def outer(pos: SourcePosition, prefix: String)(using Context): List[String] =
    if (pos.outer.exists)
       i"$prefix| This location contains code that was inlined from $pos" ::
       outer(pos.outer, prefix)
    else Nil

  /** Get the sourcelines before and after the position, as well as the offset
    * for rendering line numbers
    *
    * @return (lines before error, lines after error, line numbers offset)
    */
  def sourceLines(pos: SourcePosition, diagnosticLevel: String)(using Context): (List[String], List[String], Int) = {
    assert(pos.exists && pos.source.file.exists)
    var maxLen = Int.MinValue
    def render(offsetAndLine: (Int, String)): String = {
      val (offset, line) = offsetAndLine
      val lineNbr = pos.source.offsetToLine(offset)
      val prefix = s"${lineNbr + 1} |"
      maxLen = math.max(maxLen, prefix.length)
      val lnum = hl(diagnosticLevel)(" " * math.max(0, maxLen - prefix.length) + prefix)
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
      if (ctx.settings.color.value != "never")
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

  /** The column markers aligned under the error */
  def columnMarker(pos: SourcePosition, offset: Int, diagnosticLevel: String)(using Context): String = {
    val prefix = " " * (offset - 1)
    val padding = pos.startColumnPadding
    val carets = hl(diagnosticLevel) {
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"
    }
    s"$prefix|$padding$carets"
  }

  /** The error message (`msg`) aligned under `pos`
    *
    * @return aligned error message
    */
  def errorMsg(pos: SourcePosition, msg: String, offset: Int)(using Context): String = {
    val padding = msg.linesIterator.foldLeft(pos.startColumnPadding) { (pad, line) =>
      val lineLength = stripColor(line).length
      val maxPad = math.max(0, ctx.settings.pageWidth.value - offset - lineLength) - offset

      if (maxPad < pad.length) " " * maxPad
      else pad
    }

    msg.linesIterator
      .map { line => " " * (offset - 1) + "|" + (if line.isEmpty then "" else padding + line) }
      .mkString(EOL)
  }

  /** The separator between errors containing the source file and error type
    *
    * @return separator containing error location and kind
    */
  def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(using Context): String =
    if (pos.source != NoSourcePosition.source) hl(diagnosticLevel)({
      val pos1 = pos.nonInlined
      val file = if !pos.exists then pos1.source.file.toString else
        s"${pos1.source.file.toString}:${pos1.line + 1}:${pos1.column}"
      val errId =
        if (message.errorId ne ErrorMessageID.NoExplanationID) {
          val errorNumber = message.errorId.errorNumber
          s"[E${"0" * (3 - errorNumber.toString.length) + errorNumber}] "
        } else ""
      val kind =
        if (message.kind == "") diagnosticLevel
        else s"${message.kind} $diagnosticLevel"
      val prefix = s"-- ${errId}${kind}: $file "

      prefix +
        ("-" * math.max(ctx.settings.pageWidth.value - stripColor(prefix).length, 0))
    }) else ""

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

  def appendFilterHelp(dia: Diagnostic, sb: mutable.StringBuilder): Unit =
    import dia._
    val hasId = msg.errorId.errorNumber >= 0
    val category = dia match {
      case _: UncheckedWarning => "unchecked"
      case _: DeprecationWarning => "deprecation"
      case _: FeatureWarning => "feature"
      case _ => ""
    }
    if (hasId || category.nonEmpty)
      sb.append(EOL).append("Matching filters for @nowarn or -Wconf:")
      if (hasId)
        sb.append(EOL).append("  - id=E").append(msg.errorId.errorNumber)
        sb.append(EOL).append("  - name=").append(msg.errorId.productPrefix.stripSuffix("ID"))
      if (category.nonEmpty)
        sb.append(EOL).append("  - cat=").append(category)

  /** The whole message rendered from `msg` */
  def messageAndPos(dia: Diagnostic)(using Context): String = {
    import dia._
    val levelString = diagnosticLevel(dia)
    val sb = mutable.StringBuilder()
    val posString = posStr(pos, levelString, msg)
    if (posString.nonEmpty) sb.append(posString).append(EOL)
    if (pos.exists) {
      val pos1 = pos.nonInlined
      if (pos1.exists && pos1.source.file.exists) {
        val (srcBefore, srcAfter, offset) = sourceLines(pos1, levelString)
        val marker = columnMarker(pos1, offset, levelString)
        val err = errorMsg(pos1, msg.message, offset)
        sb.append((srcBefore ::: marker :: err :: outer(pos, " " * (offset - 1)) ::: srcAfter).mkString(EOL))
      }
      else sb.append(msg.message)
    }
    else sb.append(msg.message)
    if (dia.isVerbose)
      appendFilterHelp(dia, sb)
    sb.toString
  }

  def hl(diagnosticLevel: String)(str: String)(using Context): String = diagnosticLevel match {
    case "Info" => Blue(str).show
    case "Error" => Red(str).show
    case _ =>
      assert(diagnosticLevel.contains("Warning"))
      Yellow(str).show
  }

  def diagnosticLevel(dia: Diagnostic): String =
    dia match {
      case dia: Error => "Error"
      case dia: FeatureWarning => "Feature Warning"
      case dia: DeprecationWarning => "Deprecation Warning"
      case dia: UncheckedWarning => "Unchecked Warning"
      case dia: MigrationWarning => "Migration Warning"
      case dia: Warning => "Warning"
      case dia: Info => "Info"
    }
}
