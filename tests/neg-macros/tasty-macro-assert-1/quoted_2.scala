
import Asserts.*

object Test {
  def main(args: Array[String]): Unit = {
    macroAssert(true === "cde")
    macroAssert("acb" === "cde") // error
  }

}
