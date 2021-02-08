import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Enclosing {
  def method(): Unit = {
    abstract class AbstractLocalJSClass extends js.Object // error
  }
}
