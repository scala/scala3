import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    val a: Int = defaultOf("int")
    val b: String = defaultOf("string")
    println(a)
    println(b)
  }
}
