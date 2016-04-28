package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import scalatags.JsDom.TypedTag
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}

object IndexLayout {
  import model.Entities._
  import CustomTags._
  import MemberLayout._

  def layout(m: Entity) = m match {
    case p: Package => h1(s"Package: ${p.name}")
    case e => entity(e)
  }

  def entity(m: Entity) = div(
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
        companion(m),
        a(cls := "mdl-navigation__link", href := "#", "Source")
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
                  .flatMap(member(_, m))
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

  private def relativePath(to: Entity) =
    util.Traversing.relativePath(Index.currentEntity, to)

  def packageView = nav(
    cls := "mdl-navigation packages",
    {
      val keys: Seq[String] = Index.packages.keys.toSeq.sorted
      keys.flatMap { k =>
        val pack = Index.packages(k)
        (a(cls := "mdl-navigation__link package", href := relativePath(pack), k) ::
        pack.children.sortBy(_.name).filter(_.kind == "package").map { c =>
          a(cls := "mdl-navigation__link entity", href := relativePath(c), c.name)
        })
      }
    }
  )

  def companion(m: Entity) = {
    val pack = m.path.dropRight(1).mkString(".")
    Index.packages.get(pack)
      .flatMap(_.children.find(e => e.name == m.name && e.path.last != m.path.last))
      .map { p =>
        a(
          cls := "mdl-navigation__link",
          href := p.path.last + ".html",
          "Companion " + m.kind
        )
      }.getOrElse(span())
  }
}
