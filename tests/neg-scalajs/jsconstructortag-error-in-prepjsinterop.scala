import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native @JSGlobal class NativeJSClass extends js.Object
@js.native trait NativeJSTrait extends js.Object
@js.native @JSGlobal object NativeJSObject extends js.Object

class JSClass extends js.Object
trait JSTrait extends js.Object
object JSObject extends js.Object

object A {
  val a = js.constructorTag[NativeJSTrait] // error
  val b = js.constructorTag[NativeJSObject.type] // error

  val c = js.constructorTag[NativeJSClass with NativeJSTrait] // error
  val d = js.constructorTag[NativeJSClass { def bar: Int }] // error

  val e = js.constructorTag[JSTrait] // error
  val f = js.constructorTag[JSObject.type] // error

  val g = js.constructorTag[JSClass with JSTrait] // error
  val h = js.constructorTag[JSClass { def bar: Int }] // error

  def foo[A <: js.Any] = js.constructorTag[A] // error
  def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorTag[A] // error
}
