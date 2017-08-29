trait A {
  private class Foo


  class Inner[T <: Foo] { // error: non-private type T refers to private class Foo in its type signature
    def get: T = ???
  }
}
class B extends A {
  def foo(x: Inner[_]): Unit = {
    val a = x.get
  }
}
