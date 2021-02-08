import scala.scalajs.js
import scala.scalajs.js.annotation.*

class ScalaClass
trait ScalaTrait
object ScalaObject

object A {
  val a = js.constructorOf[ScalaClass] // error
  val b = js.constructorOf[ScalaTrait] // error
  val c = js.constructorOf[ScalaObject.type] // error
}
