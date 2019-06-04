package dotty.tastydoc.comment

import util.MemberLookup

import dotty.tastydoc.representations._

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

  implicit class StringToMarkdown(val text: String) extends AnyVal {
    def toMarkdown(origin: Representation, packages: Map[String, EmulatedPackageRepresentation]): MarkdownNode = {
      import com.vladsch.flexmark.ast.Link
      import com.vladsch.flexmark.util.ast.{Visitor, VisitHandler, NodeVisitor }

      val inlineToMarkdown = InlineToMarkdown(origin)

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

      val linkVisitor = new NodeVisitor(
        new VisitHandler(classOf[Link], new Visitor[Link] with MemberLookup {
          def queryToUrl(title: String, link: String) = makeRepresentationLink(origin, packages, Text(title), link).link match {
            case Tooltip(_) => "#"
            case LinkToExternal(_, url) => url
            case LinkToRepresentation(t: Representation) => t match {
              case e: Representation with Members => inlineToMarkdown.relativePath(t)
              case x => x.parentRepresentation.fold("#") { xpar => inlineToMarkdown.relativePath(xpar) }
            }
          }

          override def visit(link: Link) = {
            val linkUrl = link.getUrl.toString
            if (!isOuter(linkUrl) && !isRelative(linkUrl))
              link.setUrl(CharSubSequence.of(queryToUrl(link.getTitle.toString, linkUrl)))
          }
        })
      )

      linkVisitor.visit(node)
      node
    }

    def toMarkdownString(origin: Representation, packages: Map[String, EmulatedPackageRepresentation]): String =
      toMarkdown(origin, packages).show
  }

  implicit class MarkdownToHtml(val markdown: MarkdownNode) extends AnyVal {
    def show: String =
      RENDERER.render(markdown)

    def shortenAndShow: String =
      (new MarkdownShortener).shorten(markdown).show
  }

  implicit class StringToWiki(val text: String) extends AnyVal {
    def toWiki(origin: Representation, packages: Map[String, EmulatedPackageRepresentation]): Body =
      new WikiParser(origin, packages, text).document()
  }

  implicit class BodyToMarkdown(val body: Body) extends AnyVal {
    def show(origin: Representation): String = {
      val inlineToMarkdown = InlineToMarkdown(origin)

      def bodyToMarkdown(body: Body): String =
        (body.blocks map blockToMarkdown).mkString

      def listItemsToMarkdown(items: Seq[Block], level: Int = 0, ordered: Boolean = false): String ={ //TODO: Multilevel not working when different type combined, from parser?
        if(ordered){
          items.foldLeft(("", 1)){ (list, item) =>
            item match {
              case OrderedList(itemsLvl2, _) => val x = itemsLvl2; (list._1 + s"${listItemsToMarkdown(x, level + 1, true)}\n", list._2 + 1) //TODO + TOASK itemsLvl2 not found
              case UnorderedList(itemsLvl2) => val x = itemsLvl2; (list._1 + s"${listItemsToMarkdown(x, level + 1, false)}\n", list._2 + 1) //TODO + TOASK itemsLvl2 not found
              case Paragraph(inl) => (list._1 + s"${"\t"*level}${list._2}. ${inlineToMarkdown(inl)}\n", list._2 + 1)
              case block => (list._1 + s"${"\t"*level}${list._2}. ${blockToMarkdown(block)}\n", list._2 + 1)
            }
          }._1
        }else{
          items.foldLeft(""){ (list, item) =>
            item match {
              case OrderedList(itemsLvl2, _) => val x = itemsLvl2; list + s"${listItemsToMarkdown(x, level + 1, true)}\n" //TODO + TOASK itemsLvl2 not found
              case UnorderedList(itemsLvl2) => val x = itemsLvl2; list + s"${listItemsToMarkdown(x, level + 1, false)}\n" //TODO + TOASK itemsLvl2 not found
              case Paragraph(inl) => list + s"${"\t"*level}* ${inlineToMarkdown(inl)}\n"
              case block => list + s"${"\t"*level}* ${blockToMarkdown(block)}\n"
            }
          }
        }
      }

      def blockToMarkdown(block: Block): String = {
        (block match {
          case Title(in, 1)  => s"# ${inlineToMarkdown(in)}"
          case Title(in, 2)  => s"## ${inlineToMarkdown(in)}"
          case Title(in, 3)  => s"### ${inlineToMarkdown(in)}"
          case Title(in, _)  => s"#### ${inlineToMarkdown(in)}"
          case Paragraph(in) => s"${inlineToMarkdown(in)}"
          case Code(data)    => s"```scala\n$data\n```"
          case UnorderedList(items) => s"${listItemsToMarkdown(items)}"
          case OrderedList(items, listStyle) => s"${listItemsToMarkdown(items, ordered=true)}"
          case DefinitionList(items) => //TODO: Markdown equivalent
            s"<dl>${items map { case (t, d) => s"<dt>${inlineToMarkdown(t)}</dt><dd>${blockToMarkdown(d)}</dd>" } }</dl>"
          case HorizontalRule() =>
            "***"
        }) +
        "\n"
      }

      bodyToMarkdown(body)
    }
  }

  case class InlineToMarkdown(origin: Representation) {
    def apply(inl: Inline) = toMarkdown(inl)

    def relativePath(target: Representation) =
      util.traversing.relativePath(origin, target)

    def toMarkdown(inl: Inline): String = inl match {
      case Chain(items)     => (items map toMarkdown).mkString
      case Italic(in)       => s"*${toMarkdown(in)}*"
      case Bold(in)         => s"**${toMarkdown(in)}**"
      case Underline(in)    => s"__${toMarkdown(in)}__"
      case Superscript(in)  => s"<sup>${toMarkdown(in)}</sup>" //TODO Markdown equivalent
      case Subscript(in)    => s"<sub>${toMarkdown(in) }</sub>" //TODO Markdown equivalent
      case Link(raw, title) => s"[${toMarkdown(title)}]($raw)"
      case Monospace(in)    => s"`${toMarkdown(in)}`"
      case Text(text)       => text
      case Summary(in)      => toMarkdown(in)
      case HtmlTag(tag)     => tag
      case RepresentationLink(target, link) => enityLinktoMarkdown(target, link)
    }

    def enityLinktoMarkdown(target: Inline, link: LinkTo) = link match {
      case Tooltip(_) => toMarkdown(target)
      case LinkToExternal(n, url) => s"[$n]($url)"
      case LinkToRepresentation(t: Representation) => t match {
        // Representation is a package member
        case e: Representation with Members =>
          s"[${toMarkdown(target)}](${relativePath(t)})"
        // Representation is a Val / Def
        case x => x.parentRepresentation.fold(toMarkdown(target)) { xpar =>
          s"[${toMarkdown(target)}](${relativePath(xpar)}#${x.name})"
        }
      }
    }
  }
}
