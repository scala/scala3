object Test extends App {
  object O
  implicit def foo(x: O.type): String = "hello"
  val s: String = O
  implicit def bar(x: s.type): Int = s.length
  //implicit def bar2(x: String): Int = s.length
  val l: Int = s
  assert(s == "hello")
  assert(l == 5)
}