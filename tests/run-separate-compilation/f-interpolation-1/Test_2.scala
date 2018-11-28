import FQuote._

object Test {
  def main(args: Array[String]): Unit = {
    val one: Int = 1
    assert(ff"Hello $one%d!" == "Hello 1!")
    val world: String = "world"
    assert(ff"Hello $world%d!" == "`world` is not of type Int")
  }
}
