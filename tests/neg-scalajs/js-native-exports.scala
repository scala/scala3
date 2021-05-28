import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {

  @js.native
  trait Bag extends js.Any {
    val str: String
    def int: Int
    def bool(): Boolean
    def dbl(dbl: Double): Double
  }

  @js.native
  @JSGlobal("BagHolder_GlobalClass")
  final class BagHolder(val bag: Bag) extends js.Object {
    export bag.{str, int, bool, dbl} // error
  }

  @js.native
  trait BagHolderTrait extends js.Any {
    val bag: Bag
    export bag.{str, int, bool, dbl} // error
  }

  @js.native
  @JSGlobal("BagHolderModule_GlobalVar")
  object BagHolderModule extends js.Object {
    val bag: Bag = js.native
    export bag.{str, int, bool, dbl} // error
  }

  trait NonNativeBagHolderTrait extends js.Any {
    val bag: Bag
    export bag.{str, int, bool, dbl} // error
  }

}
