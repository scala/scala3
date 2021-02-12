import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport // error
  class A1 {
    @JSExport // error
    def this(x: Int) = this()
  }

  @JSExport // error
  class A2 extends js.Object

  @JSExport // error
  object A3

  @JSExport // error
  object A4 extends js.Object
}

object B {
  @JSExport // error
  class B1 {
    @JSExport // error
    def this(x: Int) = this()
  }

  @JSExport // error
  class B2 extends js.Object
}
