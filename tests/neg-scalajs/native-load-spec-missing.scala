import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
object A1 extends js.Object // error

@js.native
object A2 extends js.Object // error

@js.native
class A3 extends js.Object // error

@js.native
class A4 extends js.Object // error

object Container {
  @js.native
  val a: Int = js.native // error
}

@js.native
class X extends js.Object // error

package object subpackage {
  @js.native
  object B extends js.Object // error

  @js.native
  class B extends js.Object // error
}
