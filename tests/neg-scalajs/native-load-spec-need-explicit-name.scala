import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A1 {
  @js.native
  @JSGlobal // error
  class apply extends js.Object

  @js.native
  @JSGlobal // error
  object apply extends js.Object

  @js.native
  @JSGlobal // error
  class foo_= extends js.Object
}

object A2 {
  @js.native // error
  @JSGlobal // error
  def foo_=(x: Int): Unit = js.native

  @js.native
  @JSGlobal // error
  def apply(x: Int): Int = js.native
}

object B1 {
  @js.native
  @JSImport("bar.js") // error
  class apply extends js.Object

  @js.native
  @JSImport("bar.js") // error
  object apply extends js.Object

  @js.native
  @JSImport("bar.js") // error
  class foo_= extends js.Object
}

object B2 {
  @js.native // error
  @JSImport("bar.js") // error
  def foo_=(x: Int): Unit = js.native

  @js.native
  @JSImport("bar.js") // error
  def apply(x: Int): Int = js.native
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
