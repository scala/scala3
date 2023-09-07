import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait Bag extends js.Any {
  val str: String
}

trait NonNativeBagHolderTrait(val bag: Bag) extends js.Any // error
