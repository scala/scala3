import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native @JSGlobal
case class A1(x: Int) extends js.Object // error

@js.native @JSGlobal
case object A2 extends js.Object // error

case class B1(x: Int) extends js.Object // error

case object B2 extends js.Object // error
