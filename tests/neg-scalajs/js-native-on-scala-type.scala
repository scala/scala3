import scala.scalajs.js
import scala.scalajs.js.annotation.*

// With the correct amount of native load spec annotations

@js.native // error
@JSGlobal
class A1

@js.native // error
trait A2

@js.native // error
@JSGlobal
object A3

// With an incorrect amount of native load spec annotations

@js.native // error
class B1

@js.native // error
@JSGlobal
trait B2

@js.native // error
object B3
