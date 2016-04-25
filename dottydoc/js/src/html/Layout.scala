package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import org.scalajs.dom
import org.scalajs.dom.html.Div

object IndexLayout {
  import model.Entities._
  import CustomTags._

  def layout(ent: Entity) = entity(ent)

  def entity(m: Entity) =
    div(
      cls := "mdl-layout mdl-js-layout mdl-layout--fixed-drawer",
      div(
        cls := "mdl-layout__drawer",
        span(
          cls := "mdl-layout-title subtitle",
          m.path.dropRight(1).mkString(".")
        ),
        span(
          cls := "mdl-layout-title",
          m.name
        ),
        nav(
          cls := "related mdl-navigation",
          a(
            cls := "mdl-navigation__link",
            href := companionHref(m).map(_.path.last + ".html").getOrElse(""),
            "Companion " + m.name
          ),
          a(cls := "mdl-navigation__link", href := m.sourceUrl, "Source")
        ),
        span(
          cls := "mdl-layout-title",
          id := "docs-title",
          "Docs"
        ),
        searchView,
        packageView
      ),
      main(
        cls := "mdl-layout__content",
        div(
          cls := "page-content",
          div(raw(m.comment.fold("")(_.body))),
          m match {
            case e: Entity with Members =>
              Seq(
                h5("Members"),
                div(
                  cls := "mld-grid",
                  e.members
                    .collect { case x: Entity with Modifiers if !x.isPrivate => x}
                    .flatMap(member)
                )
              )
            case _ => ()
          }
        )
      )
    )

  def searchView = div(
    cls := "search-container",
    div(
      cls := "mdl-textfield mdl-js-textfield mdl-textfield--floating-label",
      input(cls := "mdl-textfield__input", `type` := "text", id := "search"),
      label(cls := "mdl-textfield__label", `for` := "search", "Search")
    )
  )

  def packageView = nav(
    cls := "mdl-navigation packages",
    Index.packages.keys.flatMap { k =>
      a(cls := "mdl-navigation__link package", href := "#", k) ::
      Index.packages(k).children.sortBy(_.name).map { c =>
        a(cls := "mdl-navigation__link entity", href := "#", c.name)
      }
    }.toSeq
  )

  def companionHref(m: Entity): Option[PackageMember] = {
    val pack = m.path.dropRight(1).mkString(".")
    Index.packages.get(pack)
      .flatMap(_.children.find(e => e.name == m.name && e.path.last != m.path.last))
  }

  def member(m: Entity) = {
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
        val divs = div(
          cls := "mdl-cell mdl-cell--12-col member",
          onclick := { () => toggleBetween(shortComment, and = fullComment) },
          div(
            cls := "mdl-cell mdl-cell--12-col",
            h6(m.modifiers.mkString(" ") + " " + m.kind + " " + m.name)
          ),
          shortComment,
          fullComment
        )
        Seq(divs)
      case _ => Nil
    }
  }
}
