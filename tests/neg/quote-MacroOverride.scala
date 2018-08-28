object Test {

  abstract class A {
    def f(): Unit
    rewrite def g(): Unit = ()
  }

  object B extends A {
    rewrite def f() = ~('())  // error: may not override
    override def g() = ()    // error: may not override
  }

}
