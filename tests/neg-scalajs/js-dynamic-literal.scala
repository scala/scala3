import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.Dynamic.literal

object Test {
  def test(): Unit = {
    literal.helloWorld(a = "a") // error
    literal.helloWorld("a" -> "a") // error

    val x = "string"
    literal.applyDynamicNamed(x)() // error
    literal.applyDynamic(x)() // error
  }
}
