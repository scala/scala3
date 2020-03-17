object Test {

  abstract class A {
    def f(): Unit
    inline def g(): Unit = ()
    inline def h(): Unit
  }

  object B extends A {
    override inline def f() = ()
    override def g() = () // error: cannot override final member
    def h() = () // error: is not inline, cannot implement an inline method
  }

}
