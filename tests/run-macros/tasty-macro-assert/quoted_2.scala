
import Asserts.*

object Test {
  def main(args: Array[String]): Unit = {
    macroAssert("acb" == "cde")
    macroAssert("acb" === "cde")
    macroAssert("acb" !== "acb")
  }

}
