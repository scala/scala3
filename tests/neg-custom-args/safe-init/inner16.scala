class A {
  object O {
    class B {
      val a = y        // error
    }
    class C
  }

  class Inner {
    def f(n: String) = new O.C
  }

  println(new Inner)
  println(new O.B)    // error

  val y = 10
}