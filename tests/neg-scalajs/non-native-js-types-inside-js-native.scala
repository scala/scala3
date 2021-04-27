import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal
class A extends js.Object {
  class A1 extends js.Object // error
  trait A2 extends js.Object // error
  object A3 extends js.Object // error
}

@js.native
trait B extends js.Object {
  class B1 extends js.Object // error
  trait B2 extends js.Object // error
  object B3 extends js.Object // error
}

@js.native
@JSGlobal
object C extends js.Object {
  class C1 extends js.Object // error
  trait C2 extends js.Object // ok: we can have a non-native JS trait inside a native JS object
  object C3 extends js.Object // error
}
