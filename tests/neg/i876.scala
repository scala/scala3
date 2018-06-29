object Test {
  val a: Int = 1
  implicit def foo(x: a.type): String = "hi"
  val b: Int = a

  val x: String = a // ok
  val y: String = b // error: found Int, required String
}
