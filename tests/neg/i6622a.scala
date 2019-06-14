import scala.compiletime._

object Test {

  def nonConstant: String = ""

  def main(args: Array[String]): Unit = {
    println(StringContext("abc ", nonConstant).code(println(34))) // error
  }

}
