import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal
class A extends js.Object {
  def a1_=(x: Int): Int = js.native // error
  def a2_=(x: Int, y: Int): Unit = js.native // error
  def a3_=(x: Int*): Unit = js.native // error
  def a4_=(x: Int = 1): Unit = js.native // error

  // @JSBracketAccess

  @JSBracketAccess
  def b1(): Int = js.native // error

  @JSBracketAccess
  def b2(x: Int, y: Int, z: Int): Int = js.native // error

  @JSBracketAccess
  def b3(x: Int, y: Int): Int = js.native // error

  @JSBracketAccess
  def b4(x: Int*): Int = js.native // error

  @JSBracketAccess
  def b5(x: Int = 1): Int = js.native // error

  // @JSBracketCall

  @JSBracketCall
  def c1(): Int = js.native // error

  @JSBracketCall
  def c2(xs: String*): Int = js.native // error

  @JSBracketCall
  def c3(xs: String*)(y: String): Int = js.native // error
}
