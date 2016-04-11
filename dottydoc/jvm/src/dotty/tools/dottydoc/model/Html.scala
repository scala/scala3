package dotty.tools.dottydoc
package model

import scalatags.Text.all._

object Html {
  import prickle._
  import Entities._

  private def relPath(to: String, from: Entity) =
    "../" * from.path.length + to

  def entityHtml(ent: Entity) = "<!DOCTYPE html>" + html(
    head(
      meta(charset := "utf-8"),
      meta(name := "viewport",
           content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      meta("http-equiv".attr := "x-ua-compatible", content := "ie=edge"),

      script(`type` := "text/javascript", src := relPath("static/material.min.js", ent)),
      script(`type` := "text/javascript", src := relPath("static/highlight.pack.js", ent)),
      script(`type` := "text/javascript", src := relPath("index.js", ent)),
      script(`type` := "text/javascript", src := relPath("target/scala-2.11/dottydoc-fastopt.js", ent)),
      link(rel := "stylesheet", href := relPath("static/material.min.css", ent)),
      link(rel := "stylesheet", href := relPath("static/github.css", ent)),
      link(rel := "stylesheet", href := relPath("static/index.css", ent)),
      link(rel := "stylesheet", href := "https://fonts.googleapis.com/icon?family=Material+Icons")

    ),
    body(div(id := "main-container")),
    script(raw(
      s"""|Index.currentEntity = ${Pickle.intoString(ent)};
          |dotty.tools.dottydoc.js.DottyDocJS()
          |  .main(document.getElementById("main-container"));
      """.stripMargin))
  )
}
