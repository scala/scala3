import scala.compiletime.*

object Test {

  def main(args: Array[String]): Unit = {
    println(s"abc ${codeOf(println(34))} ...")
  }


  inline def testConstant(inline msg: String): String = msg
}
