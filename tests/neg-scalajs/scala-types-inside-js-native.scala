import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal
class A extends js.Object {
  class A1 // error
  trait A2 // error
  object A3 // error
}

@js.native
trait B extends js.Object {
  class B1 // error
  trait B2 // error
  object B3 // error
}

@js.native
@JSGlobal
object C extends js.Object {
  class C1 // error
  trait C2 // error
  object C3 // error
}

// See scala-js#1891: The default parameter generates a synthetic companion object.
// The synthetic companion should be allowed, but it may not be explicit.

@js.native
@JSGlobal
object D extends js.Object {
  @js.native
  class D1(x: Int = ???) extends js.Object
  object D1 // error

  @js.native
  class D2(x: Int = ???) extends js.Object // ok, even though this creates a synthetic companion
}
