object Test {

  abstract class C[T] { def f: T }

  val x: C[Int] = new { def f = 2 }

  val y = new { val name = "Bob" }


}
