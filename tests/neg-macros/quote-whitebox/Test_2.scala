import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    val a: String = defaultOf("int") // error
    val b: Int = defaultOf("string") // error
  }
}
