package dotty.tools
package dottydoc
package model
package comment

import dotc.core.Contexts.Context
import dotc.util.Positions._
import dotty.tools.dottydoc.util.syntax._
import util.MemberLookup

object HtmlParsers {

  implicit class MarkdownToHtml(val text: String) extends AnyVal {
    def fromMarkdown(origin: Entity)(implicit ctx: Context): String = {
      import com.vladsch.flexmark.util.sequence.CharSubSequence
      import com.vladsch.flexmark.ast.{ Link, Visitor, VisitHandler, NodeVisitor }
      import com.vladsch.flexmark.parser.Parser
      import com.vladsch.flexmark.html.HtmlRenderer

      val inlineToHtml = InlineToHtml(origin)

      // TODO: split out into different step so that we can get a short summary
      val node = Parser.builder(ctx.docbase.markdownOptions)
        .build.parse(text)

      implicit def toCharSeq(str: String) = CharSubSequence.of(str)

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
          def queryToUrl(link: String) = lookup(origin, ctx.docbase.packages, link) match {
            case Tooltip(_) => "#"
            case LinkToExternal(_, url) => url
            case LinkToEntity(t: Entity) => t match {
              case e: Entity with Members => inlineToHtml.relativePath(t)
              case x => x.parent.fold("#") { xpar => inlineToHtml.relativePath(xpar) }
            }
          }

          override def visit(link: Link) = {
            val linkUrl = link.getUrl.toString
            if (!isOuter(linkUrl) && !isRelative(linkUrl))
              link.setUrl(queryToUrl(linkUrl))
          }
        })
      )

      linkVisitor.visit(node)
      HtmlRenderer.builder(ctx.docbase.markdownOptions).build().render(node)
    }
  }

  implicit class StringToWiki(val text: String) extends AnyVal {
    def toWiki(origin: Entity, packages: Map[String, Package], pos: Position): Body =
      new WikiParser(origin, packages, text, pos, origin.symbol).document()
  }

  implicit class BodyToHtml(val body: Body) extends AnyVal {
    def wikiToString(origin: Entity): String = {
      val inlineToHtml = InlineToHtml(origin)

      def bodyToHtml(body: Body): String =
        (body.blocks map blockToHtml).mkString

      def blockToHtml(block: Block): String = block match {
        case Title(in, 1)  => s"<h1>${inlineToHtml(in)}</h1>"
        case Title(in, 2)  => s"<h2>${inlineToHtml(in)}</h2>"
        case Title(in, 3)  => s"<h3>${inlineToHtml(in)}</h3>"
        case Title(in, _)  => s"<h4>${inlineToHtml(in)}</h4>"
        case Paragraph(in) => s"<p>${inlineToHtml(in)}</p>"
        case Code(data)    => s"""<pre><code class="scala">$data</code></pre>"""
        case UnorderedList(items) =>
          s"<ul>${listItemsToHtml(items)}</ul>"
        case OrderedList(items, listStyle) =>
          s"<ol class=${listStyle}>${listItemsToHtml(items)}</ol>"
        case DefinitionList(items) =>
          s"<dl>${items map { case (t, d) => s"<dt>${inlineToHtml(t)}</dt><dd>${blockToHtml(d)}</dd>" } }</dl>"
        case HorizontalRule() =>
          "<hr/>"
      }

      def listItemsToHtml(items: Seq[Block]) =
        items.foldLeft(""){ (list, item) =>
          item match {
            case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
              list + s"<li>${blockToHtml(item)}</li>"
            case Paragraph(inline) =>
              list + s"<li>${inlineToHtml(inline)}</li>"  // LIs are blocks, no need to use Ps
            case block =>
              list + s"<li>${blockToHtml(block)}</li>"
          }
      }

      bodyToHtml(body)
    }
  }

  case class InlineToHtml(origin: Entity) {
    def apply(inline: Inline) = toHtml(inline)

    def relativePath(target: Entity) =
      util.traversing.relativePath(origin, target)

    def toHtml(inline: Inline): String = inline match {
      case Chain(items)     => (items map toHtml).mkString
      case Italic(in)       => s"<i>${toHtml(in)}</i>"
      case Bold(in)         => s"<b>${toHtml(in)}</b>"
      case Underline(in)    => s"<u>${toHtml(in)}</u>"
      case Superscript(in)  => s"<sup>${toHtml(in)}</sup>"
      case Subscript(in)    => s"<sub>${toHtml(in) }</sub>"
      case Link(raw, title) => s"""<a href=$raw target="_blank">${toHtml(title)}</a>"""
      case Monospace(in)    => s"<code>${toHtml(in)}</code>"
      case Text(text)       => text
      case Summary(in)      => toHtml(in)
      case HtmlTag(tag)     => tag
      case EntityLink(target, link) => enityLinkToHtml(target, link)
    }

    def enityLinkToHtml(target: Inline, link: LinkTo) = link match {
      case Tooltip(_) => toHtml(target)
      case LinkToExternal(n, url) => s"""<a href="$url">$n</a>"""
      case LinkToEntity(t: Entity) => t match {
        // Entity is a package member
        case e: Entity with Members =>
          s"""<a href="${relativePath(t)}">${toHtml(target)}</a>"""
        // Entity is a Val / Def
        case x => x.parent.fold(toHtml(target)) { xpar =>
          s"""<a href="${relativePath(xpar)}#${x.name}">${toHtml(target)}</a>"""
        }
      }
    }
  }
}
