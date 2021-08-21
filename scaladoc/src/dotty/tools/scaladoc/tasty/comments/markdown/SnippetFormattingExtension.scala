package dotty.tools.scaladoc
package tasty.comments.markdown

import dotty.tools.scaladoc.snippets._

import com.vladsch.flexmark.formatter._
import com.vladsch.flexmark.parser._
import com.vladsch.flexmark.ext.wikilink._
import com.vladsch.flexmark.ext.wikilink.internal.WikiLinkLinkRefProcessor
import com.vladsch.flexmark.util.ast._
import com.vladsch.flexmark.util.options._
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark._

/**
 * SnippetFormattingExtension is a clone of the [[SnippetRenderingExtension]] used as a fallback strategy when the `-format` setting is set up to `md`
 */
object SnippetFormattingExtension extends Formatter.FormatterExtension:

  def rendererOptions(opt: MutableDataHolder): Unit = ()

  object ExtendedFencedCodeBlockHandler extends CustomNodeFormatter[ExtendedFencedCodeBlock]:
    override def render(node: ExtendedFencedCodeBlock, c: NodeFormatterContext, markdown: MarkdownWriter): Unit =
      markdown.append(
        SnippetRenderer.renderSnippetWithMessages(node)
      )

  object Format extends NodeFormatter:
    override def getNodeFormattingHandlers: JSet[NodeFormattingHandler[?]] =
      JSet(
        new NodeFormattingHandler(classOf[ExtendedFencedCodeBlock], ExtendedFencedCodeBlockHandler),
      )

    def getNodeClasses: JSet[Class[?]] = null

  object Factory extends NodeFormatterFactory:
    override def create(options: DataHolder): NodeFormatter = Format

  def extend(formatterBuilder: Formatter.Builder) =
    formatterBuilder.nodeFormatterFactory(Factory)
