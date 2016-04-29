package dotty.tools.dottydoc
package html

import scalatags.Text.all._

object Html {
  import prickle._
  import model.Entities._

  private def relPath(to: String, from: Entity) =
    "../" * from.path.length + to

  def entityHtml(entity: Entity, packages: Map[String, Package]) = "<!DOCTYPE html>" + html(
    head(
      meta(charset := "utf-8"),
      meta(name := "viewport",
           content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      meta("http-equiv".attr := "x-ua-compatible", content := "ie=edge"),

      //title("Dotty - " + ent.path.mkString(".")),

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
        EntityLayout(entity, packages).html
      )
    ),
    script(
      raw(s"""|dotty.tools.dottydoc.js.DottyDocJS()
              |  .main(document.getElementById("main-container"));
           """.stripMargin)
    )
  )
}
