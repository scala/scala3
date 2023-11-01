import scala.compiletime.*

object Test {

  def main(args: Array[String]): Unit = {
    println(StringContext(Seq.empty[String]*).code(Seq.empty[Any]*)) // error
  }

}
