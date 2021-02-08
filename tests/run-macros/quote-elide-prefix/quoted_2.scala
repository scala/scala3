
object Test {
  import Macro.*

  def main(args: Array[String]): Unit = {
    def test = ff"Hello ${"World"}"
    assert(test == "World")
  }
}
