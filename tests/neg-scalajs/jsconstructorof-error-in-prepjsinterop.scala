import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native @JSGlobal class NativeJSClass extends js.Object
@js.native trait NativeJSTrait extends js.Object
@js.native @JSGlobal object NativeJSObject extends js.Object

class JSClass extends js.Object
trait JSTrait extends js.Object
object JSObject extends js.Object

object A {
  val a = js.constructorOf[NativeJSTrait] // error
  val b = js.constructorOf[NativeJSObject.type] // error

  val c = js.constructorOf[NativeJSClass with NativeJSTrait] // error
  val d = js.constructorOf[NativeJSClass { def bar: Int }] // error

  val e = js.constructorOf[JSTrait] // error
  val f = js.constructorOf[JSObject.type] // error

  val g = js.constructorOf[JSClass with JSTrait] // error
  val h = js.constructorOf[JSClass { def bar: Int }] // error

  def foo[A <: js.Any] = js.constructorOf[A] // error
  def bar[A <: js.Any: scala.reflect.ClassTag] = js.constructorOf[A] // error
}
