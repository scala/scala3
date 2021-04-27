import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("not-a-valid-JS-identifier-1") // error
object A

@JSExportTopLevel("not-a-valid-JS-identifier-2") // error
class B

object C {
  @JSExportTopLevel("not-a-valid-JS-identifier-3") // error
  val a: Int = 1

  @JSExportTopLevel("not-a-valid-JS-identifier-4") // error
  var b: Int = 1

  @JSExportTopLevel("not-a-valid-JS-identifier-5") // error
  def c(): Int = 1
}

@JSExportTopLevel("") // error
object D

@JSExportTopLevel("namespaced.E") // error
object E
