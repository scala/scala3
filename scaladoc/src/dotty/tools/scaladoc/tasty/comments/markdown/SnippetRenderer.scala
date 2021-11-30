package dotty.tools.scaladoc
package tasty.comments.markdown

import com.vladsch.flexmark.html._
import util.HTML._

import dotty.tools.scaladoc.snippets._
import dotty.tools.scaladoc.util.HTML._

case class SnippetLine(content: String, lineNo: Int, classes: Set[String] = Set.empty, messages: Seq[String] = Seq.empty, attributes: Map[String, String] = Map.empty):
  def withClass(cls: String) = this.copy(classes = classes + cls)
  def withAttribute(name: String, value: String) = this.copy(attributes = attributes.updated(name, value))
  private def attributesToString: String = attributes.updated("id", lineNo).map((key, value) => s"""$key="$value"""").mkString(" ")
  def toHTML =
    val label = if messages.nonEmpty then s"""label="${messages.map(_.escapeReservedTokens).mkString("\n")}"""" else ""
    s"""<span $attributesToString class="${classes.mkString(" ")}"><span class="tooltip-container" $label></span>$content</span>"""

object SnippetRenderer:
  val hiddenStartSymbol = "//{"
  val hiddenEndSymbol = "//}"

  val importedStartSymbol = "//{i"
  val importedEndSymbol = "//i}"
  val importedRegex = """\/\/\{i:(.*)""".r

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

  private def wrapImportedSection(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val mRes = cutBetweenSymbols(importedStartSymbol, importedEndSymbol, snippetLines) {
      case (begin, mid, end) =>
        val name = importedRegex.findFirstMatchIn(mid.head.content).fold("")(_.group(1))
        begin ++ mid.drop(1).dropRight(1).map(_.withClass("hideable").withClass("include").withAttribute("name", name)) ++ wrapImportedSection(end)
    }
    mRes.getOrElse(snippetLines)

  private def wrapHiddenSymbols(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val mRes = cutBetweenSymbols(hiddenStartSymbol, hiddenEndSymbol, snippetLines) {
      case (begin, mid, end) =>
        begin ++ mid.drop(1).dropRight(1).map(_.withClass("hideable")) ++ wrapHiddenSymbols(end)
    }
    mRes.getOrElse(snippetLines)

  private def wrapCommonIndent(snippetLines: Seq[SnippetLine]): Seq[SnippetLine] =
    val nonHiddenSnippetLines = snippetLines.filter(l => !l.classes.contains("hideable"))
    nonHiddenSnippetLines.headOption.map(_.content.takeWhile(_ == ' ')).map { prefix =>
      val maxCommonIndent = nonHiddenSnippetLines.foldLeft(prefix) { (currPrefix, elem) =>
        if elem.content.startsWith(currPrefix) then currPrefix else elem.content.takeWhile(_ == ' ')
      }
      snippetLines.map { line =>
        if line.classes.contains("hideable") || maxCommonIndent.size == 0 then line
        else line.copy(content = span(cls := "hideable")(maxCommonIndent).toString + line.content.stripPrefix(maxCommonIndent))
      }
    }.getOrElse(snippetLines)

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

  private def wrapCodeLines(codeLines: Seq[String]): Seq[SnippetLine] =
    val snippetLines = codeLines.zipWithIndex.map {
      case (content, idx) => SnippetLine(content.escapeReservedTokens, idx)
    }
    wrapImportedSection
      .andThen(wrapHiddenSymbols)
      .andThen(wrapCommonIndent)
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
            s"""<span class="${compileMessageCSSClass(msg)}">${msg.message}</span>"""
          }
          .mkString("<br>")
        s"""<hr>$content"""

  private def snippetLabel(name: String): String = div(cls := "snippet-meta")(
    div(cls := "snippet-label")(name)
  ).toString

  def renderSnippetWithMessages(snippetName: Option[String], codeLines: Seq[String], messages: Seq[SnippetCompilerMessage], hasContext: Boolean): String =
    val transformedLines = wrapCodeLines.andThen(addCompileMessages(messages)).apply(codeLines).map(_.toHTML)
    val codeHTML = s"""<code class="language-scala">${transformedLines.mkString("")}</code>"""
    s"""<div class="snippet" scala-snippet ${if hasContext then "hasContext" else ""}><div class="buttons"></div><pre>$codeHTML</pre>${snippetName.fold("")(snippetLabel(_))}</div>"""

  def renderSnippetWithMessages(node: ExtendedFencedCodeBlock): String =
    renderSnippetWithMessages(
      node.name,
      node.codeBlock.getContentChars.toString.split("\n").map(_ + "\n").toSeq,
      node.compilationResult.toSeq.flatMap(_.messages),
      node.hasContext
    )
