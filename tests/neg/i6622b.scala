import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    println(StringContext("abc ").code(println(34), 34)) // error
  }

}
