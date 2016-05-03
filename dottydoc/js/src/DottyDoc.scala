package dotty.tools.dottydoc
package js

import scala.scalajs.js
import js.Dynamic.global
import js.JSApp
import js.annotation.{ JSExport, JSName }
import org.scalajs.dom
import dom.document
import dom.webworkers.Worker
import model._
import html.EntityLayout

@JSExport object DottyDocJS {
  @JSExport def main(target: dom.html.Div) = {
    document.title = "Dotty " + EntityIndex.currentEntity.path.mkString(".")
    target.appendChild(EntityLayout(EntityIndex.currentEntity).html.render)
    hljs.initHighlightingOnLoad()
  }
}

/** Library wrapper for highlighting */
@js.native object hljs extends js.Object {
  def initHighlightingOnLoad(): js.Any = js.native
}
