package dotty.tools.scaladoc
package renderers

import dotty.tools.scaladoc.tasty.comments.wiki._
import dotty.tools.scaladoc.tasty.comments.markdown.SnippetRenderer
import util.HTML._
import com.vladsch.flexmark.util.ast.{Node => MdNode}
import dotty.tools.scaladoc.tasty.comments.wiki.WikiDocElement
import dotty.tools.scaladoc.tasty.comments.markdown.DocFlexmarkRenderer

class DocRender(signatureRenderer: SignatureRenderer)(using DocContext):

  def renderDocPart(doc: DocPart): AppliedTag = doc match
    case md: MdNode => renderMarkdown(md)
    case Nil => raw("")
    case Seq(elem: WikiDocElement) => renderElement(elem)
    case list: Seq[WikiDocElement @unchecked] => div(list.map(renderElement))

  private def renderMarkdown(el: MdNode): AppliedTag =
    raw(DocFlexmarkRenderer.render(el)( (link,name) =>
      renderLink(link, default => text(if name.isEmpty then default else name)).toString
    ))

  private def listItems(items: Seq[WikiDocElement]): Seq[AppliedTag] = items match
    case Nil => Nil
    case (x :: (y: (UnorderedList | OrderedList)) :: tail) =>
      li(
        renderElement(x),
        renderElement(y)
      ) +: listItems(tail)
    case (x :: tail) =>
      li(renderElement(x)) +: listItems(tail)
  private def notSupported(name: String, content: AppliedTag): AppliedTag =
    report.warning(s"Wiki syntax does not support $name in ${signatureRenderer.currentDri.location}")
    content

  private def renderLink(target: DocLink, linkBody: String => TagArg): AppliedTag =
    target match
      case DocLink.ToDRI(dri: DRI, name: String) =>
        signatureRenderer.renderLinkContent(linkBody(name), dri)
      case DocLink.ToURL(url) => a(href := url)(linkBody(url))
      case DocLink.UnresolvedDRI(query, msg) =>
        val tooltip = s"Problem linking $query: $msg"
        signatureRenderer.unresolvedLink(linkBody(query), titleAttr :=  tooltip)

  private def renderHeader(header: Row): AppliedTag =
    tr(
      header.cells.map(c => th(c.blocks.map(renderElement)))
    )

  private def renderRow(row: Row): AppliedTag =
    tr(
      row.cells.map(c => td(c.blocks.map(renderElement)))
    )

  private def renderElement(e: WikiDocElement): AppliedTag = e match
    case Title(text, level) =>
      val content = renderElement(text)
      level match
          case 1 => h1(cls := "h600")(content)
          case 2 => h2(cls := "h300")(content)
          case 3 => h3(cls := "h200")(content)
          case 4 => h4(cls := "h100")(content)
          case 5 => h5(cls := "h50")(content)
          case 6 => h6(cls := "h50")(content)
    case Paragraph(text) => p(renderElement(text))
    case Code(data: String) => raw(SnippetRenderer.renderWikiSnippet(data))
    case HorizontalRule => hr
    case Table(header, columns, rows) =>
      table(
        thead(
          renderHeader(header)
        ),
        tbody(
          rows.map(renderRow)
        )
      )

    case UnorderedList(items) => ul(listItems(items))
    case OrderedList(items, style) => ol(listItems(items)) // TODO use style
    case Chain(items: Seq[Inline]) => span(items.map(renderElement))
    case Italic(text) => span(cls:="italic")(renderElement(text))
    case Underline(text) => span(cls:="underline")(renderElement(text))
    case Bold(text) => span(cls:="bold")(renderElement(text))
    case Monospace(text) => code(renderElement(text))
    case Superscript(text) => span(cls:="superscript")(renderElement(text))  // TODO implement style
    case Subscript(text) => span(cls:="subscript")(renderElement(text))  // TODO implement style
    case Link(target, body) =>
      renderLink(target, default => body.fold[TagArg](default)(renderElement))
    case Text(text) => raw(text.escapeReservedTokens)
    case Summary(text) => renderElement(text)
    case HtmlTag(content) => raw(content)

    case DefinitionList(items) => notSupported("DefinitionList", raw(""))

    case link: RepresentationLink =>
      notSupported("Subscript", renderElement(link.title))
