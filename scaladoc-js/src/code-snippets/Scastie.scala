package dotty.tools.scaladoc

import scala.scalajs.js
import org.scalajs.dom._

@js.native
trait Scastie extends js.Object:
    def Embedded(selector: String | Node, config: js.Dynamic): Unit = js.native
    def Embedded(selector: String | Node): Unit = js.native