
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    def foo: String = "abc"
    fun(
      foo // error
    )
  }
}
