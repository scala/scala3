object Test extends App {
  val t2 = Macro.t2(23, "foo")
  val t2a/*: (Int, String)*/ = t2
  assert(t2a == (23, "foo"))
}
