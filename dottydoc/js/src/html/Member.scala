package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div, Span}

trait MemberLayout {
  import dotty.tools.dottydoc.model._
  import dotty.tools.dottydoc.model.comment._

  def member(m: Entity, parent: Entity) = {
    def toggleBetween(short: Div, and: Div): Unit =
      if (and.style.display == "none") {
        and.style.display = "block"
        short.style.display = "none"
      } else {
        and.style.display = "none"
        short.style.display = "block"
      }

    m match {
      case m: Entity with Modifiers =>
        val shortComment = div(
          cls := "mdl-cell mdl-cell--12-col summary-comment",
          raw(m.comment.fold("")(_.short))
        ).render

        val fullComment = div(
          cls := "mdl-cell mdl-cell--12-col full-comment",
          style := "display: none;",
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
              m.modifiers.mkString(" ") + " " + m.kind
            ),
            span(
              cls := "member-name",
              m.name
            ),
            spanWith("member-type-params no-left-margin", typeParams(m)),
            spanWith("member-param-list no-left-margin", paramList(m)),
            returnValue(m, parent)
          ),
          shortComment,
          fullComment
        )
        Seq(divs)
      case x => Seq(h1("ERROR: " + x.name))
    }
  }

  def spanWith(clazz: String, contents: String) = contents match {
    case "" => None
    case _  => Some(span(cls := clazz, contents))
  }

  def paramList(m: Entity): String = m match {
    case d: Def if d.paramLists.nonEmpty =>
      d.paramLists.map { xs =>
        xs.map { tr =>
          // FIXME: should not cast like this - it won't be guaranteed to be a
          // TypeReference after Or/Types for paramlists have been implemented
          tr.title + ": " + decodeLink(tr.ref.asInstanceOf[TypeReference].tpeLink)
        }.mkString ("(", ", ", ")")
      }.mkString("")
    case _ => ""
  }

  def decodeLink: MaterializableLink => String = {
    case MaterializedLink(t, _) => t
    case NoLink(t, _) => t
    //FIXME: there should be no UnsetLinks - either MaterializedLink or NoLink
    case UnsetLink(_, q) => q
  }

  def typeParams(m: Entity): String = m match {
    case d: Def if d.typeParams.nonEmpty => d.typeParams.mkString("[", ", ", "]")
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
      def decodeTpeLink: MaterializableLink => Span  = {
        case ml: MaterializedLink =>
          println(s"received tpeLink: $ml")
          span(cls := "member-return-value", a(href := ml.target, ml.title)).render
        case un: UnsetLink =>
          span(cls := "member-return-value", shorten(un.query)).render
        case no: NoLink =>
          span(cls := "member-return-value", shorten(no.title)).render
      }

      rv match {
        case rv: TypeReference =>
          val returnValue = decodeTpeLink(rv.tpeLink)

          if (rv.paramLinks.nonEmpty) span(
            returnValue,
            "[",
            rv.paramLinks
              .map(decodeTpeLink)
              .flatMap { sp =>
                Seq(sp, span(cls := "type-separator no-left-margin", ",").render)
              }
              .dropRight(1),
            "]"
          ).render
          else returnValue

        case OrTypeReference(left, right) => span(
          cls := "member-return-value or-type",
          link(left),
          span(cls := "type-separator", "|"),
          link(right)
        ).render
        case AndTypeReference(left, right) => span(
          cls := "member-return-value and-type",
          link(left),
          span(cls := "type-separator", "&"),
          link(right)
        ).render
      }
    }

    m match {
      case rv: ReturnValue => Some(span(cls := "no-left-margin", ": ", link(rv.returnValue)))
      case _ => None
    }
  }
}
