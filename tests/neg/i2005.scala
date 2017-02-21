
object Test {
  val a = 42
  def main(args: Array[String]) = {
    val a: Int = a // error
  }
}
