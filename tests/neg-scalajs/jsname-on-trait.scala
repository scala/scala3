import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal
class Container extends js.Object {
  @JSName("A1") // error
  trait A extends js.Object

  @JSName("B1") // error
  trait B

  @JSName("C1") // error
  @js.native
  trait C extends js.Object
}
