import scala.compiletime.*

object Test {

  def main(args: Array[String]): Unit = {
    println(StringContext("abc ", "", "").code(println(34))) // error
  }

}
