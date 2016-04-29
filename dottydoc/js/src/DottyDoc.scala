package dotty.tools.dottydoc
package js

import scala.scalajs.js
import js.Dynamic.global
import js.JSApp
import js.annotation.{ JSExport, JSName }
import org.scalajs.dom
import model.Entities._
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

@JSExport object DottyDocJS {
  @JSExport def main(target: dom.html.Div) = {
    hljs.initHighlightingOnLoad()

    println("Started parsing...")
    Future(EntityIndex.packages.keys).map(println)
    println("after fututre")
  }
}

/** Library wrapper for highlighting */
@js.native object hljs extends js.Object {
  def initHighlightingOnLoad(): js.Any = js.native
}
