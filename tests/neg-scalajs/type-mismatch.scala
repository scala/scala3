import scala.scalajs.js._

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val msg = "hello"
    val n: Int = msg // error message shouldn't mention implicits from `scalajs.js.|`
  }
}
