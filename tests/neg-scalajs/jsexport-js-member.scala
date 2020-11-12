import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSGlobal
class A extends js.Object {
  @JSExport // error
  def foo: Int = js.native
}

class B extends js.Object {
  @JSExport // error
  def foo: Int = js.native
}
