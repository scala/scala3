import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait A1 extends js.Object {
  @JSGlobal("bar1") // error
  val bar1: Int = js.native
  @JSGlobal("bar2") // error
  var bar2: Int = js.native
  @JSGlobal("bar3") // error
  def bar3: Int = js.native

  @js.native
  @JSGlobal("Inner") // error
  class Inner extends js.Object

  @js.native
  @JSGlobal("Inner") // error
  object Inner extends js.Object

  @js.native
  @JSGlobal // error
  class InnerImplied extends js.Object

  @js.native
  @JSGlobal // error
  object InnerImplied extends js.Object

  @JSImport("foo.js", "babar") // error
  val babar1: Int = js.native

  @js.native
  @JSImport("foo.js", "Babar") // error
  class Babar extends js.Object

  @js.native
  @JSGlobalScope // error
  object GlobalScope extends js.Object
}

@js.native @JSGlobal
class A2 extends js.Object {
  @JSGlobal("bar1") // error
  val bar1: Int = js.native
  @JSGlobal("bar2") // error
  var bar2: Int = js.native
  @JSGlobal("bar3") // error
  def bar3: Int = js.native

  @js.native
  @JSGlobal("Inner") // error
  class Inner extends js.Object

  @js.native
  @JSGlobal("Inner") // error
  object Inner extends js.Object

  @js.native
  @JSGlobal // error
  class InnerImplied extends js.Object

  @js.native
  @JSGlobal // error
  object InnerImplied extends js.Object

  @JSImport("foo.js", "babar") // error
  val babar1: Int = js.native

  @js.native
  @JSImport("foo.js", "Babar") // error
  class Babar extends js.Object

  @js.native
  @JSGlobalScope // error
  object GlobalScope extends js.Object
}

@js.native @JSGlobal
object A3 extends js.Object {
  @JSGlobal("bar1") // error
  val bar1: Int = js.native
  @JSGlobal("bar2") // error
  var bar2: Int = js.native
  @JSGlobal("bar3") // error
  def bar3: Int = js.native

  @js.native
  @JSGlobal("Inner") // error
  class Inner extends js.Object

  @js.native
  @JSGlobal("Inner") // error
  object Inner extends js.Object

  @js.native
  @JSGlobal // error
  class InnerImplied extends js.Object

  @js.native
  @JSGlobal // error
  object InnerImplied extends js.Object

  @JSImport("foo.js", "babar") // error
  val babar1: Int = js.native

  @js.native
  @JSImport("foo.js", "Babar") // error
  class Babar extends js.Object

  @js.native
  @JSGlobalScope // error
  object GlobalScope extends js.Object
}
