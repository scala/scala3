import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobalScope // error
class A extends js.Any

@js.native
@JSGlobalScope // error
trait B extends js.Any

object Container {
  @js.native
  @JSGlobalScope // error
  class C extends js.Any

  @js.native
  @JSGlobalScope // error
  trait D extends js.Any

  @js.native
  @JSGlobalScope // error
  val a: Int = js.native

  @js.native
  @JSGlobalScope // error
  def b: Int = js.native

  @js.native
  @JSGlobalScope // error
  def c(x: Int): Int = js.native
}
