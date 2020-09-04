import scala.scalajs.js
import scala.scalajs.js.annotation._

class ScalaClass
trait ScalaTrait
object ScalaObject

object A {
  val a = js.constructorTag[ScalaClass] // error
  val b = js.constructorTag[ScalaTrait] // error
  val c = js.constructorTag[ScalaObject.type] // error
}
