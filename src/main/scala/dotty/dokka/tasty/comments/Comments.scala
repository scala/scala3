package dotty.dokka.tasty.comments

import com.vladsch.flexmark.util.ast.{ NodeIterator, Node => MarkdownNode }
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet

type Packages = Any /* Map[String, EmulatedPackageRepresentation] */
type Repr = Any

case class Comment (
  body:                    String,
  short:                   String,
  authors:                 List[String],
  see:                     List[String],
  result:                  Option[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 Option[String],
  since:                   Option[String],
  todo:                    List[String],
  deprecated:              Option[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             Option[String],
  group:                   Option[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  /** List of conversions to hide - containing e.g: `scala.Predef.FloatArrayOps` */
  hideImplicitConversions: List[String]
)

case class PreparsedComment (
  body:                    String,
  authors:                 List[String],
  see:                     List[String],
  result:                  List[String],
  throws:                  Map[String, String],
  valueParams:             Map[String, String],
  typeParams:              Map[String, String],
  version:                 List[String],
  since:                   List[String],
  todo:                    List[String],
  deprecated:              List[String],
  note:                    List[String],
  example:                 List[String],
  constructor:             List[String],
  group:                   List[String],
  groupDesc:               Map[String, String],
  groupNames:              Map[String, String],
  groupPrio:               Map[String, String],
  hideImplicitConversions: List[String],
  shortDescription:        List[String]
)

trait MarkupConversion[T] extends MemberLookup {
  protected def linkedExceptions(m: Map[String, String]): Map[String, String]
  protected def stringToMarkup(str: String): T
  protected def markupToMarkdown(t: T): String
  protected def stringToShortMarkdown(str: String): String
  protected def filterEmpty(xs: List[String]): List[T]
  protected def filterEmpty(xs: Map[String, String]): Map[String, T]

  private def single(annot: String, xs: List[String], filter: Boolean = true): Option[T] =
    (if (filter) filterEmpty(xs) else xs.map(stringToMarkup)) match {
      case x :: xs =>
        Some(x)
      case _ => None
    }

  final def parse(preparsed: PreparsedComment): Comment = Comment(
    body                    = markupToMarkdown(stringToMarkup(preparsed.body)),
    short                   = stringToShortMarkdown(preparsed.body),
    authors                 = filterEmpty(preparsed.authors).map(markupToMarkdown),
    see                     = filterEmpty(preparsed.see).map(markupToMarkdown),
    result                  = single("@result", preparsed.result).map(markupToMarkdown),
    throws                  = linkedExceptions(preparsed.throws),
    valueParams             = filterEmpty(preparsed.valueParams).view.mapValues(markupToMarkdown).toMap,
    typeParams              = filterEmpty(preparsed.typeParams).view.mapValues(markupToMarkdown).toMap,
    version                 = single("@version", preparsed.version).map(markupToMarkdown),
    since                   = single("@since", preparsed.since).map(markupToMarkdown),
    todo                    = filterEmpty(preparsed.todo).map(markupToMarkdown),
    deprecated              = single("@deprecated", preparsed.deprecated, filter = false).map(markupToMarkdown),
    note                    = filterEmpty(preparsed.note).map(markupToMarkdown),
    example                 = filterEmpty(preparsed.example).map(markupToMarkdown),
    constructor             = single("@constructor", preparsed.constructor).map(markupToMarkdown),
    group                   = single("@group", preparsed.group).map(markupToMarkdown),
    groupDesc               = filterEmpty(preparsed.groupDesc).view.mapValues(markupToMarkdown).toMap,
    groupNames              = filterEmpty(preparsed.groupNames).view.mapValues(markupToMarkdown).toMap,
    groupPrio               = filterEmpty(preparsed.groupPrio).view.mapValues(markupToMarkdown).toMap,
    hideImplicitConversions = filterEmpty(preparsed.hideImplicitConversions).map(markupToMarkdown)
  )
}

class MarkdownParser(repr: Repr, packages: Packages) extends MarkupConversion[MarkdownNode] {

  def stringToMarkup(str: String) =
    HtmlParsers.parseToMarkdown(str, repr, packages)

  def stringToShortMarkdown(str: String) =
    // TODO this parsed the doc twice, must-fix
    // str.toMarkdown(ent, packages).shortenAndShow
    "short"

  def markupToMarkdown(md: MarkdownNode) =
    HtmlParsers.renderToText(md)

  def linkedExceptions(m: Map[String, String]) = {
    // val inlineToMarkdown = InlineToMarkdown(ent)
    m.map { case (targetStr, body) =>
      val link = makeRepresentationLink(repr, packages, targetStr, targetStr)
      // (targetStr, inlineToMarkdown(link))
      (targetStr, link.title)
    }
  }

  def filterEmpty(xs: List[String]) =
    xs.map(_.trim)
      .filterNot(_.isEmpty)
      .map(stringToMarkup)

  def filterEmpty(xs: Map[String, String]) =
    xs.view.mapValues(_.trim).toMap
      .filterNot { case (_, v) => v.isEmpty }
      .view.mapValues(stringToMarkup).toMap
}
