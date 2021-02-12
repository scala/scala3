import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait A extends js.Object {
  lazy val a1: js.UndefOr[Int] = js.undefined // error

  def a(): js.UndefOr[Int] = js.undefined // error
  def b(x: Int): js.UndefOr[Int] = js.undefined // error
  def c_=(v: Int): Unit = js.undefined // error
}
