//> using options -Ycheck:all

class C {
  type A
  inline def test: A = ???
}

@main def m = {
  val c = C()
  val x: c.A = c.test
}
