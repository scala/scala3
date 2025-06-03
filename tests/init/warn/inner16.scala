class A {
  object O {
    class B {
      val a = y
    }
    class C
  }

  class Inner {
    def f(n: String) = new O.C
  }

  val inner = new Inner
  val b = new O.B

  val y = 10   // warn
}