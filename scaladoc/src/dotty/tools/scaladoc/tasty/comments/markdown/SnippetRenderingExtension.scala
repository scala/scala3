package dotty.tools.scaladoc
package tasty.comments.markdown

import dotty.tools.scaladoc.snippets._

import com.vladsch.flexmark.html._
import com.vladsch.flexmark.html.renderer._
import com.vladsch.flexmark.parser._
import com.vladsch.flexmark.ext.wikilink._
import com.vladsch.flexmark.ext.wikilink.internal.WikiLinkLinkRefProcessor
import com.vladsch.flexmark.util.ast._
import com.vladsch.flexmark.util.options._
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark._

object SnippetRenderingExtension extends HtmlRenderer.HtmlRendererExtension:
  def rendererOptions(opt: MutableDataHolder): Unit = ()
  object ExtendedFencedCodeBlockHandler extends CustomNodeRenderer[ExtendedFencedCodeBlock]:
    override def render(node: ExtendedFencedCodeBlock, c: NodeRendererContext, html: HtmlWriter): Unit =
      html.raw(
        SnippetRenderer.renderSnippetWithMessages(
          node.name,
          node.codeBlock.getContentChars.toString.split("\n").map(_ + "\n").toSeq,
          node.compilationResult.toSeq.flatMap(_.messages),
          node.hasContext
        )
      )

  object Render extends NodeRenderer:
    override def getNodeRenderingHandlers: JSet[NodeRenderingHandler[_]] =
      JSet(
        new NodeRenderingHandler(classOf[ExtendedFencedCodeBlock], ExtendedFencedCodeBlockHandler),
      )

  object Factory extends NodeRendererFactory:
    override def create(options: DataHolder): NodeRenderer = Render

  def extend(htmlRendererBuilder: HtmlRenderer.Builder, tpe: String): Unit =
    htmlRendererBuilder.nodeRendererFactory(Factory)