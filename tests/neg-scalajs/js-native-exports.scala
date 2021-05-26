import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {

  @js.native
  trait Bag extends js.Any {
    val str: String
    val int: Int
  }

  @js.native
  @JSGlobal("BagHolder_GlobalClass")
  final class BagHolder(val bag: Bag) extends js.Object {
    export bag.{str, int} // error
  }

  @js.native
  trait BagHolderTrait extends js.Any {
    val bag: Bag
    export bag.{str, int} // error
  }

  @js.native
  @JSGlobal("BagHolderModule_GlobalVar")
  object BagHolderModule extends js.Object {
    val bag: Bag = js.native
    export bag.{str, int} // error
  }

}
