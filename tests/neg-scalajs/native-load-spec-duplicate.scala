import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobalScope
@JSGlobalScope // error
object A1 extends js.Object

@js.native
@JSGlobal
@JSGlobalScope // error
object A2 extends js.Object

@js.native
@JSGlobal
@JSImport("foo.js", "A3") // error
class A3 extends js.Object

@js.native
@JSImport("foo.js", "A4")
@JSImport("bar.js", "A4") // error
class A4 extends js.Object

object Container {
  @js.native
  @JSGlobal("a")
  @JSGlobal("b") // error
  val a: Int = js.native
}

@js.native
@JSGlobal("X1")
@JSGlobal("X2") // error
@JSGlobal("X3") // error
class X extends js.Object
