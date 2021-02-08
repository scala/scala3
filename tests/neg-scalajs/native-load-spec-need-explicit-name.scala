import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {
  @js.native
  @JSGlobal // error
  class B extends js.Object

  @js.native
  @JSGlobal // error
  object C extends js.Object
}

// scala-js#2401

package object subpackage {
  @js.native
  @JSGlobal // ok
  object C extends js.Object

  @js.native
  @JSGlobal // ok
  class C extends js.Object
}
