import scala.compiletime._

object Test {

  def main(args: Array[String]): Unit = {
    println(StringContext(Seq.empty[String]:_*).code(println(34))) // error
  }

}
