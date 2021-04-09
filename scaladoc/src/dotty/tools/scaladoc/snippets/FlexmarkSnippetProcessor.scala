package dotty.tools.scaladoc
package snippets

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import collection.JavaConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock

object FlexmarkSnippetProcessor:
  def processSnippets(root: mdu.Node, debug: Boolean, checkingFunc: => SnippetChecker.SnippetCheckingFunc): mdu.Node = {
    lazy val cf: SnippetChecker.SnippetCheckingFunc = checkingFunc

    val nodes = root.getDescendants().asScala.collect {
      case fcb: mda.FencedCodeBlock => fcb
    }.toList

    nodes.foreach { node =>
      val snippet = node.getContentChars.toString
      val lineOffset = node.getStartLineNumber
      val info = node.getInfo.toString
      val argOverride =
        info.split(" ")
          .find(_.startsWith("sc:"))
          .map(_.stripPrefix("sc:"))
          .map(SCFlagsParser.parse)
          .flatMap(_.toOption)
      val snippetCompilationResult = cf(snippet, lineOffset, argOverride) match {
        case result@Some(SnippetCompilationResult(wrapped, _, _, _)) if debug =>
          val s = sequence.BasedSequence.EmptyBasedSequence()
            .append(wrapped)
            .append(sequence.BasedSequence.EOL)
          val content = mdu.BlockContent()
          content.add(s, 0)
          node.setContent(content)
          result
        case result => result
      }

      node.insertBefore(new ExtendedFencedCodeBlock(node, snippetCompilationResult))
      node.unlink()
    }

    root
  }