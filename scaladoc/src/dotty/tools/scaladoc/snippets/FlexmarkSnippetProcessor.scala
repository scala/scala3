package dotty.tools.scaladoc
package snippets

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import collection.JavaConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock
import dotty.tools.scaladoc.tasty.comments.PreparsedComment

object FlexmarkSnippetProcessor:
  extension (n: mdu.ContentNode)
    def setContentString(str: String): Unit =
      val s = sequence.BasedSequence.EmptyBasedSequence()
        .append(str)
        .append(sequence.BasedSequence.EOL)
      val content = mdu.BlockContent()
      content.add(s, 0)
      n.setContent(content)

  def processSnippets(root: mdu.Node, preparsed: Option[PreparsedComment], checkingFunc: => SnippetChecker.SnippetCheckingFunc, withContext: Boolean)(using CompilerContext): mdu.Node = {
    import SnippetChecker.Import
    lazy val cf: SnippetChecker.SnippetCheckingFunc = checkingFunc

    val nodes = root.getDescendants().asScala.collect {
      case fcb: mda.FencedCodeBlock => fcb
    }.toList

    nodes.foldLeft[Map[String, Seq[Import]]](Map()) { (importMap, node) =>
      val lineOffset = node.getStartLineNumber + preparsed.fold(0)(_.strippedLinesBeforeNo)
      val info = node.getInfo.toString.split(" ")
      if info.contains("scala") then {
        val argOverride = info
          .find(_.startsWith("sc:"))
          .map(_.stripPrefix("sc:"))
          .map(SCFlagsParser.parse)
          .flatMap {
            case Right(flags) => Some(flags)
            case Left(error) =>
              report.warning(
                s"""Error occured during parsing flags in snippet:
                    |$error""".stripMargin
              )
              None
          }
        val id = info
          .find(_.startsWith("sc-name:"))
          .map(_.stripPrefix("sc-name:"))

        val snippetImports = info
          .find(_.startsWith("sc-compile-with:"))
          .toList
          .map(_.stripPrefix("sc-compile-with:"))
          .flatMap(_.split(","))
          .flatMap { id =>
            val snippet = importMap.get(id)
            if snippet.isEmpty then
              report.warning(
                s"""Error occured during parsing compile-with in snippet:
                    |Snippet with id: $id not found.
                    |Remember that you cannot use forward reference to snippets""".stripMargin
              )
            snippet
          }
          .flatten

        val snippet = node.getContentChars.toString

        val snippetCompilationResult = cf(snippet, snippetImports, lineOffset, argOverride) match {
          case Some(result @ SnippetCompilationResult(wrapped, _, _, messages)) if !withContext =>
            node.setContentString(wrapped.original)
            val innerLineOffset = wrapped.innerLineOffset
            Some(result.copy(messages = result.messages.map {
              case m @ SnippetCompilerMessage(Some(pos), _, _) =>
                m.copy(position = Some(pos.copy(relativeLine = pos.relativeLine - innerLineOffset)))
              case m => m
            }))
          case result @ Some(SnippetCompilationResult(wrapped, _, _, _)) =>
            node.setContentString(wrapped.snippet)
            result
          case result =>
            node.setContentString(snippet)
            result
        }

        node.insertBefore(ExtendedFencedCodeBlock(id, node, snippetCompilationResult, withContext))
        node.unlink()
        id.fold(importMap)(id =>
          val entry = (id, snippetImports :+ Import(id, snippet))
          importMap + entry
        )
      } else importMap
    }

    root
  }