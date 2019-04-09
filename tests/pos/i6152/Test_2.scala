class C extends A.B {
  def f() = "hello"
}

object Main extends App {
  val c: A = new C
  println(c.f())
}
