import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal // error
trait A1 extends js.Object

@js.native
@JSGlobal("A2") // error
trait A2 extends js.Object

@js.native
@JSImport("foo.js", "A3") // error
trait A3 extends js.Object

@js.native
@JSImport("foo.js", "A4", globalFallback = "A4") // error
trait A4 extends js.Object

@js.native
@JSGlobalScope // error
trait A5 extends js.Object
