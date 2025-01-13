package foo

class C[T]:
  extension (x: T) def f(): Int = 1

object O:
  val c0: C[String] = new C[String]
  val c1: C[Int] = new C[Int]
  // Currently no import suggestions here
  2.f() // error
