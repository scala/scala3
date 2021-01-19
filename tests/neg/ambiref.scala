object test1 with

  class C with
    val x = 0
  object Test with
    val x = 1
    class D extends C with
      println(x)  // error
    new C with
      println(x)  // error

object test2 with
  def c(y: Float) =
    class D with
      val y = 2
    new D with
      println(y)  // error

object test3 with
  def c(y: Float) =
    class D with
      val y = 2
    class E extends D with
      class F with
        println(y)  // error

object test4 with

  class C with
    val x = 0
  object Test with
    val x = 1
    class D extends C with
      def x(y: Int) = 3
      val y: Int = this.x // OK
      val z: Int = x      // OK
end test4

val global = 0
class C with
  val global = 1
object D extends C with
  println(global)    // OK, since global is defined in package