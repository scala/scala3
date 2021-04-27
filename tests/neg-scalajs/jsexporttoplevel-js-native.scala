import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("A") // error
@js.native
@JSGlobal
object A extends js.Object

@JSExportTopLevel("B") // error
@js.native
trait B extends js.Object

@JSExportTopLevel("C") // error
@js.native
@JSGlobal
class C extends js.Object {
  @JSExportTopLevel("C") // error
  def this(x: Int) = this()
}
