
object Test extends App {
  class A {
    class A22(s: String) {
      def run = s
    }
  }
  val a = A()
  val x = a.A22("x")        // OK
  val x2 = a.A22.apply("X") // OK
  val x3 = a.A22.apply(_)   // OK
  val y = a.A22           // error: Cannot be used as value
  val z = a.A22.toString  // error: Cannot be used as value
  val u = A               // error: Cannot be used as value
  println(y)
}