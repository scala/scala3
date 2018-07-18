object Test {

  abstract class A {
    def f(): Unit
    transparent def g(): Unit = ()
  }

  object B extends A {
    transparent def f() = ~('())  // error: may not override
    override def g() = ()    // error: may not override
  }

}
