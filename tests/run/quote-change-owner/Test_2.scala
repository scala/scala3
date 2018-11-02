import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    assert2 {
      def bar(): Boolean = {
        println("bar")
        false
      }
      bar()
    }
  }
}
