import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    println(StringContext("abc").code(Seq.empty[Any]:_*)) // error
  }

}
