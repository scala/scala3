import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait NativeJSTrait extends js.Any

trait NonNativeJSTrait extends js.Any

object Test {
  def test(x: Any): Unit = {
    x.isInstanceOf[NativeJSTrait] // error
    x.isInstanceOf[NonNativeJSTrait] // error
  }
}
