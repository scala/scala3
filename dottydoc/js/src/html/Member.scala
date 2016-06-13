package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}

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
              cls := "member-name",
              s"""${m.modifiers.mkString(" ")} ${m.kind} ${m.name}${typeParams(m)}${paramList(m)}"""
            ),
            returnValue(m, parent)
          ),
          shortComment,
          fullComment
        )
        Seq(divs)
      case x => Seq(h1("ERROR: " + x.name))
    }
  }

  def paramList(m: Entity): String = m match {
    case d: Def if d.paramLists.nonEmpty =>
      d.paramLists.map(xs => xs map { case (x, y: UnsetLink) => s"$x: ${y.query}" } mkString ("(", ", ", ")")).mkString("")
    case _ => ""
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

    def link(rv: MaterializableLink) = rv match {
      case ml: MaterializedLink =>
        span(cls := "return-value", ": ", raw(ml.target))
      case un: UnsetLink =>
        span(cls := "return-value", ": " + shorten(un.query))
    }

    m match {
      case rv: ReturnValue => link(rv.returnValue)
      case _ => span()
    }
  }
}
