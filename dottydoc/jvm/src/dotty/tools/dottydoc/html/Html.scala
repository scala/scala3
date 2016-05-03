package dotty.tools.dottydoc
package html

import scalatags.Text.all._
import model.internal._
import model._

case class EntityPage(entity: Entity, packages: Map[String, Package]) {
  import CustomTags._
  import model.pickling._
  import prickle._

  private def relPath(to: String, from: Entity) =
    "../" * from.path.length + to

  def render = "<!DOCTYPE html>" + html(
    head(
      meta(charset := "utf-8"),
      meta(name := "viewport",
           content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      meta("http-equiv".attr := "x-ua-compatible", content := "ie=edge"),

      script(`type` := "text/javascript", src := relPath("static/material.min.js", entity)),
      script(`type` := "text/javascript", src := relPath("static/highlight.pack.js", entity)),
      script(`type` := "text/javascript", src := relPath("index.js", entity)),
      script(`type` := "text/javascript", src := relPath("target/scala-2.11/dottydoc-fastopt.js", entity)),
      link(rel := "stylesheet", href := relPath("static/material-icons.css", entity)),
      link(rel := "stylesheet", href := relPath("static/material.min.css", entity)),
      link(rel := "stylesheet", href := relPath("static/github.css", entity)),
      link(rel := "stylesheet", href := relPath("static/index.css", entity))
    ),
    body(
      div(
        id := "main-container",
        div(
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
            id := "entity-container",
            cls := "mdl-layout__content"
          )
        )
      )
    ),
    script(
      raw(s"""|UnparsedIndex.currentEntity = ${Pickle.intoString(entity)};
              |dotty.tools.dottydoc.js.DottyDocJS()
              |     .main(document.getElementById("entity-container"));
           """.stripMargin)
    )
  )

  private def filteredName(str: String) =
    str.replaceAll("\\$colon", ":")

  private def relativePath(to: Entity) =
    util.traversing.relativePath(entity, to)

  def packageView = nav(
    cls := "mdl-navigation packages",
    {
      val keys: Seq[String] = packages.keys.toSeq.sorted
      keys.flatMap { k =>
        val pack = packages(k)
        val children =
          pack.children
            .sortBy(_.name)
            .filterNot(_.kind == "package")
            .map { entity =>
              a(
                cls := "mdl-navigation__link entity",
                href := relativePath(entity),
                filteredName(entity.name)
              )
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

  def searchView = div(
    cls := "search-container",
    div(
      cls := "mdl-textfield mdl-js-textfield mdl-textfield--floating-label",
      input(cls := "mdl-textfield__input", `type` := "text", id := "search"),
      label(cls := "mdl-textfield__label", `for` := "search", "Search")
    )
  )
}
