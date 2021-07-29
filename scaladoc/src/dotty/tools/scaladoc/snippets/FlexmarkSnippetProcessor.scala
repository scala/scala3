package dotty.tools.scaladoc
package snippets

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import collection.JavaConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock

object FlexmarkSnippetProcessor:
  def processSnippets(root: mdu.Node, debug: Boolean, checkingFunc: => SnippetChecker.SnippetCheckingFunc)(using CompilerContext): mdu.Node = {
    lazy val cf: SnippetChecker.SnippetCheckingFunc = checkingFunc

    val nodes = root.getDescendants().asScala.collect {
      case fcb: mda.FencedCodeBlock => fcb
    }.toList

    nodes.foreach { node =>
      val snippet = node.getContentChars.toString
      val lineOffset = node.getStartLineNumber
      val info = node.getInfo.toString.split(" ")
      if info.contains("scala") then {
        val argOverride =
          info
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
        val snippetCompilationResult = cf(snippet, lineOffset, argOverride) match {
          case result@Some(SnippetCompilationResult(wrapped, _, _, _)) if debug =>
            val s = sequence.BasedSequence.EmptyBasedSequence()
              .append(wrapped.snippet)
              .append(sequence.BasedSequence.EOL)
            val content = mdu.BlockContent()
            content.add(s, 0)
            node.setContent(content)
            result
          case result => result
        }

        node.insertBefore(ExtendedFencedCodeBlock(node, snippetCompilationResult))
        node.unlink()
      }
    }

    root
  }