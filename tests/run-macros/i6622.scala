import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    println(code"abc ${println(34)} ...")
    println(code"abc ${println(34)}")
    println(code"${println(34)} ...")
    println(code"${println(34)}")
    println(code"...")
    println(testConstant(code""))
  }

  inline def testConstant(inline msg: String): String = msg
}
