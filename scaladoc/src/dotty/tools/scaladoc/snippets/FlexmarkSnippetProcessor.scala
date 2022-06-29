package dotty.tools.scaladoc
package snippets

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import scala.jdk.CollectionConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock
import dotty.tools.scaladoc.tasty.comments.PreparsedComment

object FlexmarkSnippetProcessor:
  def processSnippets[T <: mdu.Node](root: T, preparsed: Option[PreparsedComment], checkingFunc: => SnippetChecker.SnippetCheckingFunc)(using CompilerContext): T = {
    lazy val cf: SnippetChecker.SnippetCheckingFunc = checkingFunc

    val nodes = root.getDescendants().asScala.collect {
      case fcb: mda.FencedCodeBlock => fcb
    }.toList

    nodes.foldLeft[Map[String, String]](Map()) { (snippetMap, node) =>
      val lineOffset = node.getStartLineNumber + preparsed.fold(0)(_.strippedLinesBeforeNo)
      val info = node.getInfo.toString.split(" ")
      if info.contains("scala") then {
        val argOverride = info
          .find(_.startsWith("sc:"))
          .map(_.stripPrefix("sc:"))
          .map(SCFlagsParser.parse)
          .flatMap(_ match {
            case Right(flags) => Some(flags)
            case Left(error) =>
              report.warning(
                s"""|Error occured during parsing flags in snippet:
                    |$error""".stripMargin
              )
              None
          })
        val id = info
          .find(_.startsWith("sc-name:"))
          .map(_.stripPrefix("sc-name:"))

        val snippetImports = info
          .find(_.startsWith("sc-compile-with:"))
          .toList
          .map(_.stripPrefix("sc-compile-with:"))
          .flatMap(_.split(","))
          .flatMap { id =>
            val snippet = snippetMap.get(id)
            if snippet.isEmpty then
              report.warning(
                s"""|Error occured during parsing compile-with in snippet:
                    |Snippet with id: $id not found.
                    |Remember that you cannot use forward reference to snippets""".stripMargin
              )
            snippet
          }.mkString("\n")

        val snippet = node.getContentChars.toString

        extension (n: mdu.Node)
          def setContentString(str: String): Unit =
            val s = sequence.BasedSequence.EmptyBasedSequence()
              .append(str)
              .append(sequence.BasedSequence.EOL)
            val content = mdu.BlockContent()
            content.add(s, 0)
            node.setContent(content)

        val fullSnippet = Seq(snippetImports, snippet).mkString("\n").trim
        val snippetCompilationResult = cf(fullSnippet, lineOffset, argOverride) match {
          case Some(result @ SnippetCompilationResult(wrapped, _, _, messages)) =>
            node.setContentString(fullSnippet)
            Some(result)
          case result =>
            node.setContentString(fullSnippet)
            result
        }

        node.insertBefore(ExtendedFencedCodeBlock(id, node, snippetCompilationResult))
        node.unlink()
        id.fold(snippetMap)(id =>
          val snippetAsImport = s"""|//{i:$id
                                    |$snippet
                                    |//i}""".stripMargin
          val entry = (id, Seq(snippetImports, snippetAsImport).mkString("\n"))
          snippetMap + entry
        )
      } else snippetMap
    }

    root
  }
