package dotty.tastydoc.comment

import util.MemberLookup

import dotty.tastydoc.representations._

import java.util.{ Arrays }

import com.vladsch.flexmark.util.ast.{ Node => MarkdownNode}
import com.vladsch.flexmark.html.HtmlRenderer
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

  implicit class StringToMarkdown(val text: String) extends AnyVal {
    def toMarkdown(origin: Representation): MarkdownNode = {
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
          def queryToUrl(title: String, link: String) = makeRepresentationLink(origin, Map(), Text(title), link).link match { //TODO: Replace Map() by packages
            case Tooltip(_) => "#"
            case LinkToExternal(_, url) => url
            case LinkToRepresentation(t: Representation) => t match {
              case e: Representation with Members => inlineToMarkdown.relativePath(t)
              case x => x.parent.fold("#") { xpar => inlineToMarkdown.relativePath(xpar) }
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

    def toMarkdownString(origin: Representation): String =
      toMarkdown(origin).show
  }

  implicit class MarkdownToHtml(val markdown: MarkdownNode) extends AnyVal {
    def show: String =
      HtmlRenderer.builder(markdownOptions).build().render(markdown)

    def shortenAndShow: String =
      (new MarkdownShortener).shorten(markdown).show
  }

  implicit class StringToWiki(val text: String) extends AnyVal {
    def toWiki(origin: Representation, packages: Map[String, PackageRepresentation]): Body =
      new WikiParser(origin, packages, text).document()
  }

  // implicit class BodyToHtml(val body: Body) extends AnyVal {
  //   def show(origin: Representation): String = {
  //     val inlineToHtml = InlineToHtml(origin)

  //     def bodyToHtml(body: Body): String =
  //       (body.blocks map blockToHtml).mkString

  //     def blockToHtml(block: Block): String = block match {
  //       case Title(in, 1)  => s"<h1>${inlineToHtml(in)}</h1>"
  //       case Title(in, 2)  => s"<h2>${inlineToHtml(in)}</h2>"
  //       case Title(in, 3)  => s"<h3>${inlineToHtml(in)}</h3>"
  //       case Title(in, _)  => s"<h4>${inlineToHtml(in)}</h4>"
  //       case Paragraph(in) => s"<p>${inlineToHtml(in)}</p>"
  //       case Code(data)    => s"""<pre><code class="scala">$data</code></pre>"""
  //       case UnorderedList(items) =>
  //         s"<ul>${listItemsToHtml(items)}</ul>"
  //       case OrderedList(items, listStyle) =>
  //         s"<ol class=${listStyle}>${listItemsToHtml(items)}</ol>"
  //       case DefinitionList(items) =>
  //         s"<dl>${items map { case (t, d) => s"<dt>${inlineToHtml(t)}</dt><dd>${blockToHtml(d)}</dd>" } }</dl>"
  //       case HorizontalRule() =>
  //         "<hr/>"
  //     }

  //     def listItemsToHtml(items: Seq[Block]) =
  //       items.foldLeft(""){ (list, item) =>
  //         item match {
  //           case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
  //             list + s"<li>${blockToHtml(item)}</li>"
  //           case Paragraph(inl) =>
  //             list + s"<li>${inlineToHtml(inl)}</li>"  // LIs are blocks, no need to use Ps
  //           case block =>
  //             list + s"<li>${blockToHtml(block)}</li>"
  //         }
  //     }

  //     bodyToHtml(body)
  //   }
  // }

  // case class InlineToHtml(origin: Representation) {
  //   def apply(inl: Inline) = toHtml(inl)

  //   def relativePath(target: Representation) =
  //     util.traversing.relativePath(origin, target)

  //   def toHtml(inl: Inline): String = inl match {
  //     case Chain(items)     => (items map toHtml).mkString
  //     case Italic(in)       => s"<i>${toHtml(in)}</i>"
  //     case Bold(in)         => s"<b>${toHtml(in)}</b>"
  //     case Underline(in)    => s"<u>${toHtml(in)}</u>"
  //     case Superscript(in)  => s"<sup>${toHtml(in)}</sup>"
  //     case Subscript(in)    => s"<sub>${toHtml(in) }</sub>"
  //     case Link(raw, title) => s"""<a href=$raw target="_blank">${toHtml(title)}</a>"""
  //     case Monospace(in)    => s"<code>${toHtml(in)}</code>"
  //     case Text(text)       => text
  //     case Summary(in)      => toHtml(in)
  //     case HtmlTag(tag)     => tag
  //     case RepresentationLink(target, link) => enityLinkToHtml(target, link)
  //   }

  //   def enityLinkToHtml(target: Inline, link: LinkTo) = link match {
  //     case Tooltip(_) => toHtml(target)
  //     case LinkToExternal(n, url) => s"""<a href="$url">$n</a>"""
  //     case LinkToRepresentation(t: Representation) => t match {
  //       // Representation is a package member
  //       case e: Representation with Members =>
  //         s"""<a href="${relativePath(t)}">${toHtml(target)}</a>"""
  //       // Representation is a Val / Def
  //       case x => x.parent.fold(toHtml(target)) { xpar =>
  //         s"""<a href="${relativePath(xpar)}#${x.name}">${toHtml(target)}</a>"""
  //       }
  //     }
  //   }
  // }

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
          case Paragraph(in) => s"${inlineToMarkdown(in)}\n" //TODO: Paragraph add return?
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
        case x => x.parent.fold(toMarkdown(target)) { xpar =>
          s"[${toMarkdown(target)}](${relativePath(xpar)}#${x.name})"
        }
      }
    }
  }
}
