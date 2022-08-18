package dotty.tools.scaladoc
package tasty.comments.markdown

import com.vladsch.flexmark.html._
import com.vladsch.flexmark.html.renderer._
import com.vladsch.flexmark.parser._
import com.vladsch.flexmark.ext.wikilink._
import com.vladsch.flexmark.ext.wikilink.internal.WikiLinkLinkRefProcessor
import com.vladsch.flexmark.util.ast._
import com.vladsch.flexmark.util.options._
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark.util.html.{ AttributeImpl, Attributes }
import com.vladsch.flexmark._
import com.vladsch.flexmark.ast.FencedCodeBlock


object SectionRenderingExtension extends HtmlRenderer.HtmlRendererExtension:
  def rendererOptions(opt: MutableDataHolder): Unit = ()

  case class AnchorLink(link: String) extends BlankLine(BasedSequence.EmptyBasedSequence())
  object SectionHandler extends CustomNodeRenderer[Section]:
    val idGenerator = new HeaderIdGenerator.Factory().create()
    override def render(node: Section, c: NodeRendererContext, html: HtmlWriter): Unit =
      val Section(header, body) = node
      val id = idGenerator.getId(header.getText)
      val anchor = AnchorLink(s"#$id")
      val attributes = Attributes()
      val headerClass: String = header.getLevel match
        case 1 => "h500"
        case 2 => "h300"
        case 3 => "h200"
        case 4 => "h100"
        case _ => "h50"
      attributes.addValue(AttributeImpl.of("class", headerClass))
      val embeddedAttributes = EmbeddedAttributeProvider.EmbeddedNodeAttributes(header, attributes)
      header.prependChild(embeddedAttributes)
      header.prependChild(anchor)
      html.attr(AttributeImpl.of("id", id)).withAttr.tag("section", false, false, () => {
        c.render(header)
        body.foreach(c.render)
      })

  object AnchorLinkHandler extends CustomNodeRenderer[AnchorLink]:
    override def render(node: AnchorLink, c: NodeRendererContext, html: HtmlWriter): Unit =
      html.attr(AttributeImpl.of("href", node.link), AttributeImpl.of("class", "anchor")).withAttr.tag("a", false, false, () => ())


  object Render extends NodeRenderer:
    override def getNodeRenderingHandlers: JSet[NodeRenderingHandler[_]] =
      JSet(
        new NodeRenderingHandler(classOf[Section], SectionHandler),
        new NodeRenderingHandler(classOf[AnchorLink], AnchorLinkHandler)
      )

  object Factory extends NodeRendererFactory:
    override def create(options: DataHolder): NodeRenderer = Render

  def extend(htmlRendererBuilder: HtmlRenderer.Builder, tpe: String): Unit =
    htmlRendererBuilder.nodeRendererFactory(Factory)
