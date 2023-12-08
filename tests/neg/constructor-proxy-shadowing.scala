// scalac: -explain

object Test extends App {
  def A22(s: String): String = s
  class A33(s: String)
  object A33:
    def apply(s: String) = ???
  class A {
    class A22(s: String)
    class A33(s: String)
    val x = A22("") // error: shadowing
    val y = A33("") // error: shadowing
  }
}

class Seq(n: Int)
val x = Seq(3) // error: shadowing
