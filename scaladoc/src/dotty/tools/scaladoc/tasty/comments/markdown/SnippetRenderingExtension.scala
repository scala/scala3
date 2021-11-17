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
import com.vladsch.flexmark.ast.FencedCodeBlock

/**
 * SnippetRenderingExtension is responsible for running an analysis for scala codeblocks in the static documentation/scaladoc comments.
 * For each codeblock we run compiler to check whether snippet works in the newest scala version and to produce rich html codeblocks with
 * compiler warnings/errors for IDE-like live experience.
 */
object SnippetRenderingExtension extends HtmlRenderer.HtmlRendererExtension:
  def rendererOptions(opt: MutableDataHolder): Unit = ()
  object ExtendedFencedCodeBlockHandler extends CustomNodeRenderer[ExtendedFencedCodeBlock]:
    override def render(node: ExtendedFencedCodeBlock, c: NodeRendererContext, html: HtmlWriter): Unit =
      html.raw(
        SnippetRenderer.renderSnippetWithMessages(node)
      )

  object FencedCodeBlockHandler extends CustomNodeRenderer[FencedCodeBlock]:
    override def render(node: FencedCodeBlock, c: NodeRendererContext, html: HtmlWriter): Unit =
      html.raw("""<div class="snippet">""")
      c.delegateRender()
      html.raw("""</div>""")

  object Render extends NodeRenderer:
    override def getNodeRenderingHandlers: JSet[NodeRenderingHandler[_]] =
      JSet(
        new NodeRenderingHandler(classOf[ExtendedFencedCodeBlock], ExtendedFencedCodeBlockHandler),
        new NodeRenderingHandler(classOf[FencedCodeBlock], FencedCodeBlockHandler)
      )

  object Factory extends NodeRendererFactory:
    override def create(options: DataHolder): NodeRenderer = Render

  def extend(htmlRendererBuilder: HtmlRenderer.Builder, tpe: String): Unit =
    htmlRendererBuilder.nodeRendererFactory(Factory)
