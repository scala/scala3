package dotty.tools.scaladoc.tasty.comments.markdown

import com.vladsch.flexmark.html._

import dotty.tools.scaladoc.snippets._
import dotty.tools.scaladoc.util.HTML._

case class SnippetLine(content: String, lineNo: Int, classes: Set[String] = Set.empty, messages: Seq[String] = Seq.empty):
  def withClass(cls: String) = this.copy(classes = classes + cls)
  def toHTML =
    val label = if messages.nonEmpty then s"""label="${messages.map(_.escapeReservedTokens).mkString("\n")}"""" else ""
    s"""<span id="$lineNo" class="${classes.mkString(" ")}" $label>$content</span>"""

object SnippetRenderer:
  private def compileMessageCSSClass(msg: SnippetCompilerMessage) = msg.level match
    case MessageLevel.Info => "snippet-info"
    case MessageLevel.Warning => "snippet-warn"
    case MessageLevel.Error => "snippet-error"
    case MessageLevel.Debug => "snippet-debug"

  private def cutBetweenSymbols[A](
    startSymbol: String,
    endSymbol: String,
    snippetLines: Seq[SnippetLine]
  )(
    f: (Seq[SnippetLine], Seq[SnippetLine], Seq[SnippetLine]) => A
  ): Option[A] =
    for {
      startIdx <- snippetLines.zipWithIndex.find(_._1.content.contains(startSymbol)).map(_._2)
      endIdx <- snippetLines.zipWithIndex.find(_._1.content.contains(endSymbol)).map(_._2)
      (tmp, end) = snippetLines.splitAt(endIdx+1)
      (begin, mid) = tmp.splitAt(startIdx)
    } yield f(begin, mid, end)

  private def wrapHiddenSymbols(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val mRes = cutBetweenSymbols("//{", "//}", snippetLines) {
      case (begin, mid, end) =>
        begin ++ mid.drop(1).dropRight(1).map(_.withClass("hideable")) ++ wrapHiddenSymbols(end)
    }
    mRes.getOrElse(snippetLines)

  private def wrapLineInBetween(startSymbol: Option[String], endSymbol: Option[String], line: SnippetLine): SnippetLine =
    val startIdx = startSymbol.map(s => line.content.indexOf(s))
    val endIdx = endSymbol.map(s => line.content.indexOf(s))
    (startIdx, endIdx) match
      case (Some(idx), None) =>
        val (code, comment) = line.content.splitAt(idx)
        comment match
          case _ if code.forall(_.isWhitespace) =>
            line.withClass("hideable")
          case _ if comment.last == '\n' =>
            line.copy(content = code + s"""<span class="hideable">${comment.dropRight(1)}</span>${"\n"}""")
          case _  =>
            line.copy(content = code + s"""<span class="hideable">$comment</span>""")
      case (None, Some(idx)) =>
        val (comment, code) = line.content.splitAt(idx+endSymbol.get.size)
        comment match
          case _ if code.forall(_.isWhitespace) =>
            line.withClass("hideable")
          case _ =>
            line.copy(content = s"""<span class="hideable">$comment</span>""" + code)
      case (Some(startIdx), Some(endIdx)) =>
        val (tmp, end) = line.content.splitAt(endIdx+endSymbol.get.size)
        val (begin, comment) = tmp.splitAt(startIdx)
        line.copy(content = begin + s"""<span class="hideable">$comment</span>""" + end)
      case _ => line

  private def wrapSingleLineComments(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    snippetLines.map { line =>
      line.content.indexOf("//") match
        case -1 => line
        case idx =>
          wrapLineInBetween(Some("//"), None, line)
    }

  private def wrapMultiLineComments(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val mRes = cutBetweenSymbols("/*", "*/", snippetLines) {
      case (begin, mid, end) if mid.size == 1 =>
        val midRedacted = mid.map(wrapLineInBetween(Some("/*"), Some("*/"), _))
        begin ++ midRedacted ++ end
      case (begin, mid, end) =>
        val midRedacted =
          mid.take(1).map(wrapLineInBetween(Some("/*"), None, _))
            ++ mid.drop(1).dropRight(1).map(_.withClass("hideable"))
            ++ mid.takeRight(1).map(wrapLineInBetween(None, Some("*/"), _))
        begin ++ midRedacted ++ wrapMultiLineComments(end)
    }
    mRes.getOrElse(snippetLines)

  private def wrapCodeLines(codeLines: Seq[String]): Seq[SnippetLine] =
    val snippetLines = codeLines.zipWithIndex.map {
      case (content, idx) => SnippetLine(content.escapeReservedTokens, idx)
    }
    wrapHiddenSymbols
      .andThen(wrapSingleLineComments)
      .andThen(wrapMultiLineComments)
      .apply(snippetLines)

  private def addCompileMessages(messages: Seq[SnippetCompilerMessage])(codeLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val messagesDict = messages.filter(_.position.nonEmpty).groupBy(_.position.get.relativeLine).toMap[Int, Seq[SnippetCompilerMessage]]
    codeLines.map { line =>
      messagesDict.get(line.lineNo) match
        case None => line
        case Some(messages) =>
          val classes = List(
            messages.find(_.level == MessageLevel.Error).map(compileMessageCSSClass),
            messages.find(_.level == MessageLevel.Warning).map(compileMessageCSSClass),
            messages.find(_.level == MessageLevel.Info).map(compileMessageCSSClass)
          ).flatten
          line.copy(classes = line.classes ++ classes.toSet ++ Set("tooltip"), messages = messages.map(_.message))
    }

  private def messagesHTML(messages: Seq[SnippetCompilerMessage]): String =
    if messages.isEmpty
      then ""
      else
        val content = messages
          .map { msg =>
            s"""<span class="${compileMessageCSSClass(msg)}">${msg.getSummary}</span>"""
          }
          .mkString("<br>")
        s"""<hr>$content"""

  def renderSnippetWithMessages(codeLines: Seq[String], messages: Seq[SnippetCompilerMessage]): String =
    val transformedLines = wrapCodeLines.andThen(addCompileMessages(messages)).apply(codeLines).map(_.toHTML)
    val codeHTML = s"""<code class="language-scala">${transformedLines.mkString("")}<pre></pre></code>"""
    s"""<div class="snippet"><pre>$codeHTML</pre></div>"""