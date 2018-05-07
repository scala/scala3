import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    Macros.foo() // error: Failed to evaluate inlined quote. Caused by timeout (3000 ms).
  }
}
