import scala.scalajs.js
import scala.scalajs.js.annotation.*

abstract class A1 extends js.Object {
  val a1: js.UndefOr[Int] = 5
  val a2: js.UndefOr[Int]

  def b1: js.UndefOr[Int] = 5
  def b2: js.UndefOr[Int]
}

trait A2 extends A1 {
  override val a1: js.UndefOr[Int] = js.undefined // error
  override val a2: js.UndefOr[Int] = js.undefined

  override def b1: js.UndefOr[Int] = js.undefined // error
  override def b2: js.UndefOr[Int] = js.undefined
}

@js.native @JSGlobal
class B1 extends js.Object {
  val a: js.UndefOr[Int] = js.native
  def b: js.UndefOr[Int] = js.native
}

trait B2 extends B1 {
  override val a: js.UndefOr[Int] = js.undefined // error
  override def b: js.UndefOr[Int] = js.undefined // error
}

@js.native
trait C1 extends js.Object {
  val a: js.UndefOr[Int] = js.native
  def b: js.UndefOr[Int] = js.native
}

@js.native @JSGlobal
class C2 extends C1

trait C3 extends C2 {
  override val a: js.UndefOr[Int] = js.undefined // error
  override def b: js.UndefOr[Int] = js.undefined // error
}
