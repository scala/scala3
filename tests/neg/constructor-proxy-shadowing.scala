
object Test extends App {
  def A22(s: String): String = s
  class A {
    class A22(s: String) {
      def run = s
    }
    val x = A22("") // error: shadowing
  }
}