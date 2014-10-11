
object Test2 {

  trait A {
    type X
    protected[this] def f(x: X): X = x
  }

  trait B extends A {
    type X <: B
    def kaboom = f(new B {})
  }
}
