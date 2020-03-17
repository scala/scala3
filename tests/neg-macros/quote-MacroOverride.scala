object Test {

  abstract class A {
    def f(): Unit
    inline def g(): Unit = ()
  }

  object B extends A {
    override inline def f() = ()
    override def g() = () // error: is not inline, cannot override an inline method
  }

}
