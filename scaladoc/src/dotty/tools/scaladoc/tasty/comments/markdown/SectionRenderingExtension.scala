package dotty.tools.scaladoc
package tasty.comments.markdown

import com.vladsch.flexmark.html.*
import com.vladsch.flexmark.html.renderer.*
import com.vladsch.flexmark.html.renderer.NodeRenderingHandler.CustomNodeRenderer
import com.vladsch.flexmark.parser.*
import com.vladsch.flexmark.ext.wikilink.*
import com.vladsch.flexmark.ext.wikilink.internal.WikiLinkLinkRefProcessor
import com.vladsch.flexmark.util.ast.*
import com.vladsch.flexmark.util.options.*
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark.*
import com.vladsch.flexmark.ast.FencedCodeBlock

import scala.collection.mutable
import com.vladsch.flexmark.util.data.MutableDataHolder
import com.vladsch.flexmark.util.html.Attributes
import com.vladsch.flexmark.util.html.AttributeImpl
import com.vladsch.flexmark.util.data.DataHolder
import com.vladsch.flexmark.util.html.Attribute
import com.vladsch.flexmark.util.html.MutableAttributes


object SectionRenderingExtension extends HtmlRenderer.HtmlRendererExtension:
  def rendererOptions(opt: MutableDataHolder): Unit = ()

  case class AnchorLink(link: String) extends BlankLine(BasedSequence.EmptyBasedSequence())
  class SectionHandler extends CustomNodeRenderer[Section]:
    val idGenerator = new HeaderIdGenerator.Factory().create()
    idGenerator.setResolveDupes(true)
    override def render(node: Section, c: NodeRendererContext, html: HtmlWriter): Unit =
      val Section(header, body) = node

      /* #19524 flexmark's `HeaderIdGenerator` does not appear to be thread-safe,
       * so we protect its usage with a full `synchronize`.
       */
      val id = idGenerator.synchronized {
        idGenerator.getId(header.getText)
      }

      val anchor = AnchorLink(s"#$id")
      val headerClass: String = header.getLevel match
        case 1 => "h500"
        case 2 => "h500"
        case 3 => "h400"
        case 4 => "h300"
        case _ => "h50"
      val attributes = MutableAttributes()
      attributes.addValue("class", headerClass)
      val embeddedAttributes = EmbeddedAttributeProvider.EmbeddedNodeAttributes(header, attributes)
      header.prependChild(embeddedAttributes)
      header.prependChild(anchor)
      html.attr("id", id).withAttr.tag("section", false, false, () => {
        c.render(header)
        body.foreach(c.render)
      })

  object AnchorLinkHandler extends CustomNodeRenderer[AnchorLink]:
    override def render(node: AnchorLink, c: NodeRendererContext, html: HtmlWriter): Unit =
      html.attr(AttributeImpl.of("href", node.link), AttributeImpl.of("class", "anchor")).withAttr.tag("a", false, false, () => ())


  object Render extends NodeRenderer:
    override def getNodeRenderingHandlers: JSet[NodeRenderingHandler[?]] =
      JSet(
        new NodeRenderingHandler(classOf[Section], new SectionHandler),
        new NodeRenderingHandler(classOf[AnchorLink], AnchorLinkHandler)
      )

  object Factory extends NodeRendererFactory:
    override def apply(options: DataHolder): NodeRenderer = Render

  def extend(htmlRendererBuilder: HtmlRenderer.Builder, tpe: String): Unit =
    htmlRendererBuilder.nodeRendererFactory(Factory)
