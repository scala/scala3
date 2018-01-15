object Test {

  abstract class A {
    def f(): Unit
    inline def g(): Unit = ()
  }

  class B extends A {
    inline def f() = ~('())  // error: may not override
    override def g() = ()    // error: may not override
  }

}
