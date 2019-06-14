import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    assert(code"abc ${println(34)} ..." == "abc println(34) ...")
    assert(code"abc ${println(34)}" == "abc println(34)")
    assert(code"${println(34)} ..." == "println(34) ...")
    assert(code"${println(34)}" == "println(34)")
    assert(code"..." == "...")
    assert(testConstant(code"") == "")
  }

  inline def testConstant(inline msg: String): String = msg
}
