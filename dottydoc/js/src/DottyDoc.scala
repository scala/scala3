package dotty.tools.dottydoc
package js

import scala.scalajs.js
import js.Dynamic.global
import js.JSApp
import js.annotation.{ JSExport, JSName }
import org.scalajs.dom
import model.Entities._

@JSExport object DottyDocJS {
  @JSExport def main(target: dom.html.Div) = {
    global.document.title = "Dotty " + ParsedIndex.currentEntity.path.mkString(".")
    target.appendChild(html.Index.layout(ParsedIndex.currentEntity).render)
    hljs.initHighlightingOnLoad()
  }
}

object ParsedIndex {
  import prickle._
  import model.Entities._

  val packages: Map[String, Package] = Unpickle[Map[String, Package]]
    .fromString(js.JSON.stringify(Index.packages))
    .toOption
    .getOrElse(Map.empty)

  val currentEntity: Entity = Unpickle[Entity]
    .fromString(js.JSON.stringify(Index.currentEntity))
    .toOption
    .getOrElse(NonEntity)
}

@js.native
object Index extends js.Object {
  def packages: js.Any = js.native

  def currentEntity: js.Any = js.native
}

@js.native
object hljs extends js.Object {
  def initHighlightingOnLoad(): js.Any = js.native
}
