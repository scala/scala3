import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("A") // error
abstract class A

abstract class B(x: Int) {
  @JSExportTopLevel("B") // error
  def this() = this(5)
}

@JSExportTopLevel("C") // error
trait C

@JSExportTopLevel("D") // ok
abstract class D extends js.Object

@JSExportTopLevel("E") // error
trait E extends js.Object

@JSExportTopLevel("F") // error
@js.native @JSGlobal
abstract class F extends js.Object

@JSExportTopLevel("G") // error
@js.native
trait G extends js.Object
