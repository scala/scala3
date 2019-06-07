object Test {

  abstract class A {
    def f(): Unit
    inline def g(): Unit = ()
  }

  object B extends A {
    override inline def f() = () // error: method f of type (): Unit is an inline method, must override at least one concrete method
    override def g() = ()
  }

}
