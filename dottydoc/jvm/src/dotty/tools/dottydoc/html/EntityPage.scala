package dotty.tools.dottydoc
package html

import scalatags.Text.all._
import model._

case class EntityPage(entity: Entity, packages: Map[String, Package]) {
  import model.json._
  import util.internal.setters._

  private def relPath(to: String, from: Entity) = {
    val len = (from.path.length - 1) + (from match {
      case _: Package => 1
      case _ => 0
    })

    "../" * len + to
  }

  def render = "<!DOCTYPE html>" + html(
    head(
      meta(charset := "utf-8"),
      meta(name := "viewport",
           content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      meta("http-equiv".attr := "x-ua-compatible", content := "ie=edge"),

      script(`type` := "text/javascript", src := relPath("static/material.min.js", entity)),
      script(`type` := "text/javascript", src := relPath("static/highlight.pack.js", entity)),
      script(`type` := "text/javascript", src := relPath("index.js", entity)),
      script(`type` := "text/javascript", src := relPath("static/dottydoc-fastopt.js", entity)),
      link(rel := "stylesheet", href := relPath("static/material-icons.css", entity)),
      link(rel := "stylesheet", href := relPath("static/material.min.css", entity)),
      link(rel := "stylesheet", href := relPath("static/github.css", entity)),
      link(rel := "stylesheet", href := relPath("static/index.css", entity))
    ),
    body(div(id := "main-container")),
    script(
      raw(s"""|UnparsedIndex.currentEntity = ${entity.flat.json};
              |dotty.tools.dottydoc.js.DottyDocJS()
              |     .main(document.getElementById("main-container"));
           """.stripMargin)
    )
  )
}
