package dotty.tools
package dottydoc
package model
package comment

import dotty.tools.dottydoc.util.syntax._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.config.Printers.dottydoc

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

private[comment] case class ParsedComment (
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

object MarkdownComment extends util.MemberLookup {
  import HtmlParsers._

  def apply(parsed: ParsedComment, ent: Entity)(implicit ctx: Context): Comment = {
    val inlineToHtml = InlineToHtml(ent)
    def linkedExceptions(m: Map[String, String]): Map[String, String] = {
      m.map { case (targetStr, body) =>
        val link = lookup(ent, ctx.docbase.packages, targetStr)
        val entityLink = EntityLink(Monospace(Text(targetStr)), link)
        (targetStr, inlineToHtml(entityLink))
      }
    }

    Comment(
      body                    = parsed.body.toMarkdownString(ent),
      short                   = parsed.body.toMarkdown(ent).shortenAndShow,
      authors                 = filterEmpty(parsed.authors).map(_.toMarkdownString(ent)),
      see                     = filterEmpty(parsed.see).map(_.toMarkdownString(ent)),
      result                  = single("@result", parsed.result).map(_.toMarkdownString(ent)),
      throws                  = linkedExceptions(parsed.throws).mapValues(_.toMarkdownString(ent)),
      valueParams             = filterEmpty(parsed.valueParams).mapValues(_.toMarkdownString(ent)),
      typeParams              = filterEmpty(parsed.typeParams).mapValues(_.toMarkdownString(ent)),
      version                 = single("@version", parsed.version).map(_.toMarkdownString(ent)),
      since                   = single("@since", parsed.since).map(_.toMarkdownString(ent)),
      todo                    = filterEmpty(parsed.todo).map(_.toMarkdownString(ent)),
      deprecated              = single("@deprecated", parsed.deprecated, filter = false).map(_.toMarkdownString(ent)),
      note                    = filterEmpty(parsed.note).map(_.toMarkdownString(ent)),
      example                 = filterEmpty(parsed.example).map(_.toMarkdownString(ent)),
      constructor             = single("@constructor", parsed.constructor).map(_.toMarkdownString(ent)),
      group                   = single("@group", parsed.group).map(_.toMarkdownString(ent)),
      groupDesc               = filterEmpty(parsed.groupDesc).mapValues(_.toMarkdownString(ent)),
      groupNames              = filterEmpty(parsed.groupNames).mapValues(_.toMarkdownString(ent)),
      groupPrio               = filterEmpty(parsed.groupPrio).mapValues(_.toMarkdownString(ent)),
      hideImplicitConversions = filterEmpty(parsed.hideImplicitConversions).map(_.toMarkdownString(ent))
    )
  }

  private def filterEmpty(xs: List[String]) =
    xs.map(_.trim).filterNot(_.isEmpty)

  private def filterEmpty(xs: Map[String, String]) =
    xs.mapValues(_.trim).filterNot { case (_, v) => v.isEmpty }

  private def single(annot: String, xs: List[String], filter: Boolean = true): Option[String] =
    (if (filter) filterEmpty(xs) else xs) match {
      case x :: xs =>
        if (xs.nonEmpty) dottydoc.println(s"Only allowed to have a single annotation for $annot")
        Some(x)
      case _ => None
    }
}

object WikiComment extends util.MemberLookup {
  import HtmlParsers._

  def apply(parsed: ParsedComment, ent: Entity, pos: Position)(implicit ctx: Context): Comment = {
    val inlineToHtml = InlineToHtml(ent)
    val packages = ctx.docbase.packages
    val parsedBody = parsed.body.toWiki(ent, packages, pos).show(ent)

    def linkedExceptions(m: Map[String, Body]): Map[String, Body] = {
      m.map { case (targetStr, body) =>
        val link = lookup(ent, ctx.docbase.packages, targetStr)
        val newBody = body match {
          case Body(List(Paragraph(Chain(content)))) =>
            val descr = Text(" ") +: content
            val entityLink = EntityLink(Monospace(Text(targetStr)), link)
            Body(List(Paragraph(Chain(entityLink +: descr))))
          case _ => body
        }
        (targetStr, newBody)
      }
    }

    def toWiki(str: String): Body = str.toWiki(ent, packages, pos)
    def toString(body: Body): String = body.show(ent)

    def filterEmpty(xs: List[String]): List[Body] =
      xs.map(toWiki).filterNot(_.blocks.isEmpty)

    def filterEmptyMap(xs: Map[String, String]): Map[String, Body] =
      xs.mapValues(toWiki).filterNot { case (_, v) => v.blocks.isEmpty }

    def single(annot: String, xs: List[String], filter: Boolean = false): Option[String] = {
      val head = (if (filter) filterEmpty(xs) else xs.map(toWiki)) match {
        case x :: xs =>
          if (xs.nonEmpty) dottydoc.println(s"Only allowed to have a single annotation for $annot")
          Some(x)
        case _ => None
      }

      head.map(toString)
    }

    Comment(
      body                    = parsedBody,
      short                   = parsedBody,
      authors                 = filterEmpty(parsed.authors).map(toString),
      see                     = filterEmpty(parsed.see).map(toString),
      result                  = single("@result", parsed.result),
      throws                  = linkedExceptions(parsed.throws.mapValues(toWiki)).mapValues(toString),
      valueParams             = filterEmptyMap(parsed.valueParams).mapValues(toString),
      typeParams              = filterEmptyMap(parsed.typeParams).mapValues(toString),
      version                 = single("@version", parsed.version),
      since                   = single("@since", parsed.since),
      todo                    = filterEmpty(parsed.todo).map(toString),
      deprecated              = single("@deprecated", parsed.deprecated, filter = false),
      note                    = filterEmpty(parsed.note).map(toString),
      example                 = filterEmpty(parsed.example).map(toString),
      constructor             = single("@constructor", parsed.constructor),
      group                   = single("@group", parsed.group),
      groupDesc               = filterEmptyMap(parsed.groupDesc).mapValues(toString),
      groupNames              = filterEmptyMap(parsed.groupNames).mapValues(toString),
      groupPrio               = filterEmptyMap(parsed.groupPrio).mapValues(toString),
      hideImplicitConversions = filterEmpty(parsed.hideImplicitConversions).map(toString)
    )
  }
}
