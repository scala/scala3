package dotty.tools.scaladoc
package tasty.comments
package markdown

import com.vladsch.flexmark.html._
import com.vladsch.flexmark.html.renderer._
import com.vladsch.flexmark.parser._
import com.vladsch.flexmark.ext.wikilink._
import com.vladsch.flexmark.ext.wikilink.internal.WikiLinkLinkRefProcessor
import com.vladsch.flexmark.util.ast._
import com.vladsch.flexmark.ast._
import com.vladsch.flexmark.util.options._
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark._

import dotty.tools.scaladoc.snippets._
import scala.collection.JavaConverters._

class DocLinkNode(
  val target: DocLink,
  val body: String,
  seq: BasedSequence
) extends WikiNode(seq, false, false, false, false)

case class ExtendedFencedCodeBlock(
  name: Option[String],
  codeBlock: ast.FencedCodeBlock,
  compilationResult: Option[SnippetCompilationResult]
) extends BlankLine(codeBlock.getContentChars())

case class Section(
  header: Heading,
  body: List[Node]
) extends BlankLine(header.getContentChars())

class DocFlexmarkParser(resolveLink: String => DocLink) extends Parser.ParserExtension:

  def parserOptions(opt: MutableDataHolder): Unit = () // noop

  class Factory extends LinkRefProcessorFactory:
    override def getBracketNestingLevel(options: DataHolder) = 1
    override def getWantExclamationPrefix(options: DataHolder) = false
    override def create(doc: Document): LinkRefProcessor =
      new WikiLinkLinkRefProcessor(doc):
        override def createNode(nodeChars: BasedSequence): Node =
          val chars = nodeChars.toString.substring(2, nodeChars.length - 2)
          val (target, body) = DocFlexmarkParser.splitWikiLink(chars)
          new DocLinkNode(resolveLink(target), body, nodeChars)


  override def extend(parserBuilder: Parser.Builder) =
    parserBuilder.linkRefProcessorFactory(new Factory)

object DocFlexmarkParser {
  def splitWikiLink(chars: String): (String, String) =
    // split on a space which is not backslash escaped (regex uses "zero-width negative lookbehind")
    chars.split("(?<!(?<!\\\\)\\\\) ", /*max*/ 2) match {
      case Array(target) => (target, "")
      case Array(target, userText) => (target, userText)
    }
}

case class DocFlexmarkRenderer(renderLink: (DocLink, String) => String)
  extends HtmlRenderer.HtmlRendererExtension:
    def rendererOptions(opt: MutableDataHolder): Unit = () // noop

    object Handler extends CustomNodeRenderer[DocLinkNode]:
      override def render(node: DocLinkNode, c: NodeRendererContext, html: HtmlWriter): Unit =
        html.raw(renderLink(node.target, node.body))

    object Render extends NodeRenderer:
      override def getNodeRenderingHandlers: JSet[NodeRenderingHandler[_]] =
        JSet(
          new NodeRenderingHandler(classOf[DocLinkNode], Handler),
        )

    object Factory extends NodeRendererFactory:
      override def create(options: DataHolder): NodeRenderer = Render

    def extend(htmlRendererBuilder: HtmlRenderer.Builder, tpe: String): Unit =
      htmlRendererBuilder.nodeRendererFactory(Factory)

object DocFlexmarkRenderer:
  def render(node: Node)(renderLink: (DocLink, String) => String) =
    val opts = MarkdownParser.mkMarkdownOptions(
      Seq(
        DocFlexmarkRenderer(renderLink),
        SnippetRenderingExtension,
        SectionRenderingExtension
      )
    )
    HtmlRenderer.builder(opts).build().render(node)
