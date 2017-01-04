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

    val parsedBody = parsed.body.fromMarkdown(ent)
    Comment(
      body                    = parsedBody,
      short                   = parsedBody,
      authors                 = filterEmpty(parsed.authors).map(_.fromMarkdown(ent)),
      see                     = filterEmpty(parsed.see).map(_.fromMarkdown(ent)),
      result                  = single("@result", parsed.result).map(_.fromMarkdown(ent)),
      throws                  = linkedExceptions(parsed.throws).mapValues(_.fromMarkdown(ent)),
      valueParams             = filterEmpty(parsed.valueParams).mapValues(_.fromMarkdown(ent)),
      typeParams              = filterEmpty(parsed.typeParams).mapValues(_.fromMarkdown(ent)),
      version                 = single("@version", parsed.version).map(_.fromMarkdown(ent)),
      since                   = single("@since", parsed.since).map(_.fromMarkdown(ent)),
      todo                    = filterEmpty(parsed.todo).map(_.fromMarkdown(ent)),
      deprecated              = single("@deprecated", parsed.deprecated, filter = false).map(_.fromMarkdown(ent)),
      note                    = filterEmpty(parsed.note).map(_.fromMarkdown(ent)),
      example                 = filterEmpty(parsed.example).map(_.fromMarkdown(ent)),
      constructor             = single("@constructor", parsed.constructor).map(_.fromMarkdown(ent)),
      group                   = single("@group", parsed.group).map(_.fromMarkdown(ent)),
      groupDesc               = filterEmpty(parsed.groupDesc).mapValues(_.fromMarkdown(ent)),
      groupNames              = filterEmpty(parsed.groupNames).mapValues(_.fromMarkdown(ent)),
      groupPrio               = filterEmpty(parsed.groupPrio).mapValues(_.fromMarkdown(ent)),
      hideImplicitConversions = filterEmpty(parsed.hideImplicitConversions).map(_.fromMarkdown(ent))
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
    val parsedBody = parsed.body.toWiki(ent, packages, pos).wikiToString(ent)

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
    def toString(body: Body): String = body.wikiToString(ent)

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
