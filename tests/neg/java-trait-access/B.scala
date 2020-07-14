package pkg {
  trait B extends A
  class C extends B
}

object Test {
  def test1: Unit = {
    val c = new pkg.C
    c.foo() // OK
    val b: pkg.B = c
    b.foo() // error: Unable to emit reference to method foo in class A, class A is not accessible in object Test
  }
}
