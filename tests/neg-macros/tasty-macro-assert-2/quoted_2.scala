
import Asserts.*

object Test {
  def main(args: Array[String]): Unit = {
    macroAssert(false !== "acb")
    macroAssert("acb" !== "acb") // error
  }

}
