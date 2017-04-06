trait A {
  private type Foo = Int


  class Inner[T <: Foo] { // error: non-private type T refers to private type Foo in its type signature
    def get: T = ???
  }
}
class B extends A {
  def foo(x: Inner[_]): Unit = {
    val a = x.get // error: cannot resolve reference to type B(B.this).Foo
  }
}
