package dotty.dokka.tasty.comments

import java.util.{ Arrays }

import com.vladsch.flexmark.util.ast.{ Node => MarkdownNode}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.sequence.CharSubSequence
import com.vladsch.flexmark.parser.ParserEmulationProfile
import com.vladsch.flexmark.ext.gfm.tables.TablesExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.yaml.front.matter.YamlFrontMatterExtension
import com.vladsch.flexmark.util.options.{ DataHolder, MutableDataSet }

object HtmlParsers {

  val markdownOptions: DataHolder =
    new MutableDataSet()
      .setFrom(ParserEmulationProfile.KRAMDOWN.getOptions)
      .set(Parser.EXTENSIONS, Arrays.asList(
        TablesExtension.create(),
        TaskListExtension.create(),
        AutolinkExtension.create(),
        AnchorLinkExtension.create(),
        EmojiExtension.create(),
        YamlFrontMatterExtension.create(),
        StrikethroughExtension.create()
      ))
      .set(EmojiExtension.ROOT_IMAGE_PATH,
        "https://github.global.ssl.fastly.net/images/icons/emoji/")

  val RENDERER = Formatter.builder(markdownOptions).build()

  def parseToMarkdown(
    text: String,
    origin: Repr,
    packages: Packages
  ): MarkdownNode = {
    import com.vladsch.flexmark.ast.Link
    import com.vladsch.flexmark.util.ast.{Visitor, VisitHandler, NodeVisitor}

    // val inlineToMarkdown = InlineToMarkdown(origin)

    val node = Parser.builder(markdownOptions)
      .build.parse(text)

    def isOuter(url: String) =
      url.startsWith("http://") ||
      url.startsWith("https://") ||
      url.startsWith("ftp://") ||
      url.startsWith("ftps://")

    def isRelative(url: String) =
      url.startsWith("../") ||
      url.startsWith("./")

    // val linkVisitor = new NodeVisitor(
    //   new VisitHandler(classOf[Link], new Visitor[Link] with MemberLookup {
    //     def queryToUrl(title: String, link: String) = makeRepresentationLink(origin, packages, Text(title), link).link match {
    //       case Tooltip(_) => "#"
    //       case LinkToExternal(_, url) => url
    //       case LinkToRepresentation(t: Representation) => t match {
    //         case e: Representation with Members => inlineToMarkdown.relativePath(t)
    //         case x => x.parentRepresentation.fold("#") { xpar => inlineToMarkdown.relativePath(xpar) }
    //       }
    //     }

    //     override def visit(link: Link) = {
    //       val linkUrl = link.getUrl.toString
    //       if (!isOuter(linkUrl) && !isRelative(linkUrl))
    //         link.setUrl(CharSubSequence.of(queryToUrl(link.getTitle.toString, linkUrl)))
    //     }
    //   })
    // )

    // linkVisitor.visit(node)
    node
  }

  def renderToText(node: MarkdownNode): String =
    RENDERER.render(node)
}
