object A {
  type T

  def foo(a: T) = ()

}

object B {
  type T
  A.foo(??? : T) // error


}
