package dotty.tools.dottydoc
package html

import scalatags.Text.all._
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}
import model.Entities._

case class EntityLayout(entity: Entity, packages: Map[String, Package]) {
  import CustomTags._
  import MemberLayout._

  def html = div(
    cls := "mdl-layout mdl-js-layout mdl-layout--fixed-drawer",
    div(
      cls := "mdl-layout__drawer",
      span(
        cls := "mdl-layout-title subtitle",
        entity.path.dropRight(1).mkString(".")
      ),
      span(
        cls := "mdl-layout-title",
        entity match {
          case p: Package => p.name.split("\\.").last
          case e => e.name
        }
      ),
      nav(
        cls := "related mdl-navigation",
        companion,
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
        div(raw(entity.comment.fold("")(_.body))),
        entity match {
          case e: Entity with Members =>
            Seq(
              h5("Members"),
              div(
                cls := "mld-grid",
                e.members
                  .collect { case x: Entity with Modifiers if !x.isPrivate => x}
                  .flatMap(member(_, entity))
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
    util.Traversing.relativePath(entity, to)

  private def filteredName(str: String) =
    str.replaceAll("\\$colon", ":")

  def packageView = nav(
    cls := "mdl-navigation packages",
    {
      val keys: Seq[String] = packages.keys.toSeq.sorted
      keys.flatMap { k =>
        val pack = packages(k)
        val children =
          pack.children.sortBy(_.name).filterNot(_.kind == "package").map { c =>
            a(cls := "mdl-navigation__link entity", href := relativePath(c), filteredName(c.name))
          }

        if (children.length > 0)
          a(cls := "mdl-navigation__link package", href := relativePath(pack), filteredName(k)) :: children
        else Nil
      }
    }
  )

  def companion = {
    val pack = entity.path.dropRight(1).mkString(".")
    packages.get(pack)
      .flatMap { p =>
        p.children.find(e => e.name == entity.name && e.path.last != entity.path.last)
      }
      .map { c =>
        a(
          cls := "mdl-navigation__link",
          href := c.path.last + ".html",
          "Companion " + c.kind
        )
      }.getOrElse(span())
  }
}
