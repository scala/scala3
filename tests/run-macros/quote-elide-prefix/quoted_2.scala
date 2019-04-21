
object Test {
  import Macro._

  def main(args: Array[String]): Unit = {
    def test = ff"Hello ${"World"}"
    assert(test == "World")
  }
}
