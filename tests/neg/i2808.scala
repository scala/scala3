class C {}

object Test {
  def foo1() = { println("foo1") ; 5 }
  val c = new C
  foo1() m1_: c // error
}
