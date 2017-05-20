package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import core.Decorators._
import printing.Highlighting.{Blue, Red}
import printing.SyntaxHighlighting
import diagnostic.{ErrorMessageID, Message, MessageContainer, NoExplanation}
import diagnostic.messages._
import util.SourcePosition
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
  def outer(pos: SourcePosition, prefix: String)(implicit ctx: Context): List[String] =
    if (pos.outer.exists) {
       s"$prefix| This location is in code that was inlined at ${pos.outer}" ::
       outer(pos.outer, prefix)
    } else Nil

  /** Get the sourcelines before and after the position, as well as the offset
    * for rendering line numbers
    *
    * @return (lines before error, lines after error, line numbers offset)
    */
  def sourceLines(pos: SourcePosition)(implicit ctx: Context): (List[String], List[String], Int) = {
    var maxLen = Int.MinValue
    def render(offsetAndLine: (Int, String)): String = {
      val (offset, line) = offsetAndLine
      val lineNbr = pos.source.offsetToLine(offset)
      val prefix = s"${lineNbr + 1} |"
      maxLen = math.max(maxLen, prefix.length)
      val lnum = Red(" " * math.max(0, maxLen - prefix.length) + prefix).show
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
        SyntaxHighlighting(pos.linesSlice).toArray
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
  def columnMarker(pos: SourcePosition, offset: Int)(implicit ctx: Context): String = {
    val prefix = " " * (offset - 1)
    val padding = pos.startColumnPadding
    val carets = Red {
      if (pos.startLine == pos.endLine)
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"
    }
    s"$prefix|$padding${carets.show}"
  }

  /** The error message (`msg`) aligned under `pos`
    *
    * @return aligned error message
    */
  def errorMsg(pos: SourcePosition, msg: String, offset: Int)(implicit ctx: Context): String = {
    val padding = msg.lines.foldLeft(pos.startColumnPadding) { (pad, line) =>
      val lineLength = stripColor(line).length
      val maxPad = math.max(0, ctx.settings.pageWidth.value - offset - lineLength) - offset

      if (maxPad < pad.length) " " * maxPad
      else pad
    }

    msg.lines
      .map { line => " " * (offset - 1) + "|" + padding + line}
      .mkString(sys.props("line.separator"))
  }

  /** The separator between errors containing the source file and error type
    *
    * @return separator containing error location and kind
    */
  def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(implicit ctx: Context): String =
    if (pos.exists) Blue({
      val file = s"${pos.source.file.toString}:${pos.line + 1}:${pos.column}"
      val errId =
        if (message.errorId ne ErrorMessageID.NoExplanationID) {
          val errorNumber = message.errorId.errorNumber()
          s"[E${"0" * (3 - errorNumber.toString.length) + errorNumber}] "
        } else ""
      val kind =
        if (message.kind == "") diagnosticLevel
        else s"${message.kind} $diagnosticLevel"
      val prefix = s"-- ${errId}${kind}: $file "

      prefix +
        ("-" * math.max(ctx.settings.pageWidth.value - stripColor(prefix).length, 0))
    }).show else ""

  /** Explanation rendered under "Explanation" header */
  def explanation(m: Message)(implicit ctx: Context): String = {
    val sb = new StringBuilder(
      hl"""|
           |${Blue("Explanation")}
           |${Blue("===========")}"""
    )
    sb.append('\n').append(m.explanation)
    if (m.explanation.lastOption != Some('\n')) sb.append('\n')
    sb.toString
  }

  /** The whole message rendered from `msg` */
  def messageAndPos(msg: Message, pos: SourcePosition, diagnosticLevel: String)(implicit ctx: Context): String = {
    val sb = mutable.StringBuilder.newBuilder
    sb.append(posStr(pos, diagnosticLevel, msg)).append('\n')
    if (pos.exists) {
      val (srcBefore, srcAfter, offset) = sourceLines(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg.msg, offset)
      sb.append((srcBefore ::: marker :: err :: outer(pos, " " * (offset - 1)) ::: srcAfter).mkString("\n"))
    } else sb.append(msg.msg)
    sb.toString
  }

  def diagnosticLevel(cont: MessageContainer): String =
    cont match {
      case m: Error => "Error"
      case m: FeatureWarning => "Feature Warning"
      case m: DeprecationWarning => "Deprecation Warning"
      case m: UncheckedWarning => "Unchecked Warning"
      case m: MigrationWarning => "Migration Warning"
      case m: Warning => "Warning"
      case m: Info => "Info"
    }
}
