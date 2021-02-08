import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait A extends js.Object {
  val a1: js.UndefOr[Int] = 5 // error
  val a2: Int = 5 // error

  def b1: js.UndefOr[Int] = 5 // error
  def b2: Int = 5 // error

  var c1: js.UndefOr[Int] = 5 // error
  var c2: Int = 5 // error

  def d1(x: Int): Int = x + 1 // error
  def d2[A](x: A): A = x // error
}
