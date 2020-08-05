package dotty.dokka.tasty.comments

import org.jetbrains.dokka.model.{doc => dkkd}

import com.vladsch.flexmark.util.{ast => mdu}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet

type Packages = Any /* Map[String, EmulatedPackageRepresentation] */

import scala.tasty.Reflection
class Repr(val r: Reflection)(val sym: r.Symbol)

case class Comment (
  body:                    dkkd.DocTag,
  short:                   String,
  authors:                 List[dkkd.DocTag],
  see:                     List[dkkd.DocTag],
  result:                  Option[dkkd.DocTag],
  throws:                  Map[String, dkkd.DocTag],
  valueParams:             Map[String, dkkd.DocTag],
  typeParams:              Map[String, dkkd.DocTag],
  version:                 Option[dkkd.DocTag],
  since:                   Option[dkkd.DocTag],
  todo:                    List[dkkd.DocTag],
  deprecated:              Option[dkkd.DocTag],
  note:                    List[dkkd.DocTag],
  example:                 List[dkkd.DocTag],
  constructor:             Option[dkkd.DocTag],
  group:                   Option[dkkd.DocTag],
  groupDesc:               Map[String, dkkd.DocTag],
  groupNames:              Map[String, dkkd.DocTag],
  groupPrio:               Map[String, dkkd.DocTag],
  /** List of conversions to hide - containing e.g: `scala.Predef.FloatArrayOps` */
  hideImplicitConversions: List[dkkd.DocTag]
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
  shortDescription:        List[String],
  syntax:                  List[String],
)

trait MarkupConversion[T] extends MemberLookup {
  protected def linkedExceptions(m: Map[String, String]): Map[String, dkkd.DocTag]
  protected def stringToMarkup(str: String): T
  protected def markupToDokka(t: T): dkkd.DocTag
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
    body                    = markupToDokka(stringToMarkup(preparsed.body)),
    short                   = stringToShortMarkdown(preparsed.body),
    authors                 = filterEmpty(preparsed.authors).map(markupToDokka),
    see                     = filterEmpty(preparsed.see).map(markupToDokka),
    result                  = single("@result", preparsed.result).map(markupToDokka),
    throws                  = linkedExceptions(preparsed.throws),
    valueParams             = filterEmpty(preparsed.valueParams).view.mapValues(markupToDokka).toMap,
    typeParams              = filterEmpty(preparsed.typeParams).view.mapValues(markupToDokka).toMap,
    version                 = single("@version", preparsed.version).map(markupToDokka),
    since                   = single("@since", preparsed.since).map(markupToDokka),
    todo                    = filterEmpty(preparsed.todo).map(markupToDokka),
    deprecated              = single("@deprecated", preparsed.deprecated, filter = false).map(markupToDokka),
    note                    = filterEmpty(preparsed.note).map(markupToDokka),
    example                 = filterEmpty(preparsed.example).map(markupToDokka),
    constructor             = single("@constructor", preparsed.constructor).map(markupToDokka),
    group                   = single("@group", preparsed.group).map(markupToDokka),
    groupDesc               = filterEmpty(preparsed.groupDesc).view.mapValues(markupToDokka).toMap,
    groupNames              = filterEmpty(preparsed.groupNames).view.mapValues(markupToDokka).toMap,
    groupPrio               = filterEmpty(preparsed.groupPrio).view.mapValues(markupToDokka).toMap,
    hideImplicitConversions = filterEmpty(preparsed.hideImplicitConversions).map(markupToDokka)
  )
}

class MarkdownCommentParser(repr: Repr, packages: Packages)
    extends MarkupConversion[mdu.Document] {

  def stringToMarkup(str: String) =
    HtmlParsers.parseToMarkdown(str, repr, packages)

  def stringToShortMarkdown(str: String) =
    // TODO this parsed the doc twice, must-fix
    // str.toMarkdown(ent, packages).shortenAndShow
    "short"

  def markupToDokka(md: mdu.Document) =
    MarkdownConverter(repr.r)(repr.sym).convertDocument(md)

  def linkedExceptions(m: Map[String, String]) = {
    // val inlineToMarkdown = InlineToMarkdown(ent)
    m.map { case (targetStr, body) =>
      // val link = makeRepresentationLink(repr, packages, targetStr, targetStr)
      // (targetStr, inlineToMarkdown(link))
      (targetStr, dkk.text(body))
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

case class WikiCommentParser(repr: Repr, packages: Packages)
    extends MarkupConversion[wiki.Body] {

  def stringToMarkup(str: String) =
    wiki.Parser(str).document()

  def stringToShortMarkdown(str: String) =
    // val parsed = stringToMarkup(str)
    // parsed.summary.getOrElse(parsed).show(ent)
    null

  def markupToDokka(body: wiki.Body) = wiki.Converter(repr.r)(repr.sym).convertBody(body)

  def linkedExceptions(m: Map[String, String]) = {
    m.view.mapValues(stringToMarkup).toMap.map { case (targetStr, body) =>
      // import wiki._
      // val link = lookup(Some(ent), packages, targetStr)
      // val newBody = body match {
      //   case Body(List(Paragraph(Chain(content)))) =>
      //     val descr = Text(" ") +: content
      //     val link = makeRepresentationLink(ent, packages, targetStr, targetStr)
      //     Body(List(Paragraph(Chain(link +: descr))))
      //   case _ => body
      // }
      // (targetStr, newBody.show(ent))
      (targetStr, wiki.Converter(repr.r)(repr.sym).convertBody(body))
    }
  }

  def filterEmpty(xs: List[String]) =
    xs.map(stringToMarkup)

  def filterEmpty(xs: Map[String,String]) =
    xs.view.mapValues(stringToMarkup).toMap
      .filterNot { case (_, v) => v.blocks.isEmpty }

}
