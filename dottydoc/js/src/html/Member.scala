package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div, Span}

trait MemberLayout {
  import js.model._
  import js.model.ops._

  def member(m: Entity, parent: Entity) = {
    def toggleBetween(short: Div, and: Div): Unit =
      if (and.style.display == "none") {
        and.style.display = "block"
        short.style.display = "none"
      } else {
        and.style.display = "none"
        short.style.display = "block"
      }

    m.kind match {
      case "class" | "case class" | "object" | "trait" | "def" | "val" =>
        val entity = m.asInstanceOf[Entity with Modifiers]
        val shortComment = div(
          cls := "mdl-cell mdl-cell--12-col summary-comment",
          raw(m.comment.fold("")(_.short))
        ).render

        val fullComment = div(
          cls := "mdl-cell mdl-cell--12-col full-comment",
          style := "display: none;",
          fromImplicitSpan(m),
          raw(m.comment.fold("")(_.body))
        ).render

        val hasLongerFullComment = m.comment.fold(false) { c =>
          c.short.length + 5 < c.body.length
        }

        val divs = div(
          cls :=
            s"""
            mdl-cell mdl-cell--12-col member
            ${if (hasLongerFullComment) "member-fullcomment" else ""}
            """,
          onclick := { () => toggleBetween(shortComment, and = fullComment) },
          div(
            cls := "mdl-cell mdl-cell--12-col member-definition",
            span(
              cls := "member-modifiers-kind",
              entity.modifiers.mkString(" ") + " " + m.kind
            ),
            span(
              cls := { if (m.addedImplicitly) "member-name implicitly-added" else "member-name" },
              m.name
            ),
            spanWith("member-type-params no-left-margin", typeParams(m)),
            span(cls := "member-param-list no-left-margin", paramList(m)),
            returnValue(entity, parent)
          ),
          shortComment,
          fullComment
        )
        Seq(divs)
      case _ => Seq(h1("ERROR: " + m.name))
    }
  }

  def fromImplicitSpan(m: Entity) = m.foldImplicitlyAdded { ref =>
    span(
      "Implicitly added from: ",
      referenceToLinks(ref)
    )
  }.getOrElse(span())

  def spanWith(clazz: String, contents: String) = contents match {
    case "" => None
    case _  => Some(span(cls := clazz, contents))
  }

  def paramList(m: Entity): Span = m.kind match {
    case "def" =>
      val d = m.asInstanceOf[Def]
      if (d.paramLists.nonEmpty)
        span(
          cls := "member-param-lists",
          d.paramLists.map { xs =>
            span(
              cls := "param-list",
              "(",
              span(cls := "is-implicit no-left-margin", if (xs.isImplicit) "implicit " else ""),
              xs.list.flatMap { tr =>
                Seq(
                  span(cls := "param-name", tr.title).render,
                  span(cls := "type-separator no-left-margin", if (tr.isByName) ": =>" else ":").render,
                  span(if (tr.ref.kind == "FunctionReference" && tr.isRepeated) "(" else "").render,
                  span(referenceToLinks(tr.ref)).render,
                  span(if (tr.ref.kind == "FunctionReference" && tr.isRepeated) ")*" else if (tr.isRepeated) "*" else "").render,
                  span(cls := "type-separator no-left-margin", ",").render
                )
              }.toList.dropRight(1),
              ")"
            ).render
          }.toList
        ).render
      else span().render
    case _ => span().render
  }

  def referenceToLinks(ref: Reference): Span = {
    def linkToAnchor(link: MaterializableLink) = link.kind match {
      case "MaterializedLink" =>
        val (t, url) = (link.asInstanceOf[MaterializedLink].title, link.asInstanceOf[MaterializedLink].target)
        a(href := url, t).render
      case "NoLink" => span(link.title).render

      case "UnsetLink" =>
        println(s"UnsetLink found: $link")
        span(link.title).render
    }

    ref.kind match {
      case "TypeReference" =>
        val tref = ref.asInstanceOf[TypeReference]
        val infixTypes = "<:<" :: "=:=" :: Nil
        if (tref.paramLinks.length == 2 && infixTypes.contains(tref.title)) span(
          referenceToLinks(tref.paramLinks(0)),
          span(cls := "type-separator no-left-margin"),
          linkToAnchor(tref.tpeLink),
          span(cls := "type-separator no-left-margin"),
          referenceToLinks(tref.paramLinks(1))
        ).render
        else if (tref.paramLinks.nonEmpty) span(
          linkToAnchor(tref.tpeLink),
          "[",
          tref
            .paramLinks
            .map(referenceToLinks)
            .flatMap(link => Seq(link, span(cls := "type-separator no-left-margin", ",").render))
            .toList.dropRight(1),
          "]"
        ).render
      else span(linkToAnchor(tref.tpeLink)).render

      case "OrTypeReference" =>
        val (left, right) = (ref.asInstanceOf[OrTypeReference].left, ref.asInstanceOf[OrTypeReference].right)
        span(
          referenceToLinks(left),
          span(cls := "type-separator", "|"),
          referenceToLinks(right)
        ).render

      case "AndTypeReference" =>
        val (left, right) = (ref.asInstanceOf[AndTypeReference].left, ref.asInstanceOf[AndTypeReference].right)
        span(
          referenceToLinks(left),
          span(cls := "type-separator", "&"),
          referenceToLinks(right)
        ).render

      case "BoundsReference" =>
        val (low, high) = (ref.asInstanceOf[BoundsReference].low, ref.asInstanceOf[BoundsReference].high)
        span(
          referenceToLinks(low),
          span(cls := "type-separator", "<:"),
          referenceToLinks(high)
        ).render

      case "FunctionReference" => {
        val func = ref.asInstanceOf[FunctionReference]
        span(
          cls := "no-left-margin",
          if (func.args.length > 1) "(" else "",
          if (func.args.isEmpty)
            span("()")
          else func
              .args
              .map(referenceToLinks)
              .flatMap(link => Seq(link, span(cls := "type-separator no-left-margin", ",").render)).init.toList,
          if (func.args.length > 1) ") => " else " => ",
          referenceToLinks(func.returnValue)
        ).render
      }

      case "TupleReference" => {
        val func = ref.asInstanceOf[TupleReference]
        span(
          cls := "no-left-margin",
          "(",
          func
            .args
            .map(referenceToLinks)
            .flatMap(link => Seq(link, span(cls := "type-separator no-left-margin", ",").render)).init.toList,
          ")"
        ).render
      }
    }
  }

  def typeParams(m: Entity): String = m.kind match {
    case "def" =>
      val d = m.asInstanceOf[Def]
      if (d.typeParams.nonEmpty)
        d.typeParams.mkString("[", ", ", "]")
      else ""
    case _ => ""
  }

  def returnValue(m: Entity with Modifiers, parent: Entity) = {
    // shortens: "Option.this.A" => "A"
    def shorten(s: String): String = s.split('.').toList match {
      case x :: Nil => x
      case x :: xs if x == parent.name => xs.last
      case xs => s
    }

    def link(rv: Reference): Span = {
      def decodeTpeLink(link: MaterializableLink): Span = link.kind match {
        case "MaterializedLink" =>
          val ml = link.asInstanceOf[MaterializedLink]
          span(cls := "member-return-value", a(href := ml.target, ml.title)).render
        case "UnsetLink" =>
          val un = link.asInstanceOf[UnsetLink]
          span(cls := "member-return-value", shorten(un.query)).render
        case "NoLink" =>
          val no = link.asInstanceOf[NoLink]
          span(cls := "member-return-value", shorten(no.title)).render
      }

      rv.kind match {
        case "TypeReference" =>
          val trv = rv.asInstanceOf[TypeReference]
          val returnValue = decodeTpeLink(trv.tpeLink)

          if (trv.paramLinks.nonEmpty) span(
            returnValue,
            "[",
            trv.paramLinks
              .map(link)
              .flatMap { sp =>
                Seq(sp, span(cls := "type-separator no-left-margin", ",").render)
              }
              .toList.dropRight(1),
            "]"
          ).render
          else returnValue

        case "OrTypeReference" =>
          val (left, right) = (rv.asInstanceOf[OrTypeReference].left, rv.asInstanceOf[OrTypeReference].right)
          span(
            cls := "member-return-value or-type",
            link(left),
            span(cls := "type-separator", "|"),
            link(right)
          ).render
        case "AndTypeReference" =>
          val (left, right) = (rv.asInstanceOf[AndTypeReference].left, rv.asInstanceOf[AndTypeReference].right)
          span(
            cls := "member-return-value and-type",
            link(left),
            span(cls := "type-separator", "&"),
            link(right)
          ).render

        case "BoundsReference" =>
          val (low, high) = (rv.asInstanceOf[BoundsReference].low, rv.asInstanceOf[BoundsReference].high)
          span(
            link(low),
            span(cls := "type-separator", "<:"),
            link(high)
          ).render
        case "FunctionReference" =>
          val func = rv.asInstanceOf[FunctionReference]
          span(
            cls := "no-left-margin",
            if (func.args.length > 1) "(" else "",
            if (func.args.isEmpty)
              span("()")
            else func
              .args
              .map(link)
              .flatMap(link => Seq(link, span(cls := "type-separator no-left-margin", ",").render)).init.toList,
            if (func.args.length > 1) ") => " else " => ",
            link(func.returnValue)
          ).render

        case "TupleReference" => {
          val func = rv.asInstanceOf[TupleReference]
          span(
            cls := "no-left-margin",
            "(",
            func
              .args
              .map(link)
              .flatMap(link => Seq(link, span(cls := "type-separator no-left-margin", ",").render)).init.toList,
            ")"
          ).render
        }
      }
    }

    m.kind match {
      case "def" =>
        val rv = m.asInstanceOf[ReturnValue]
        Some(span(cls := "no-left-margin", ": ", link(rv.returnValue)))
      case _ => None
    }
  }
}
