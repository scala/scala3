inline trait A[T](val x: T):
  def foo: T = x

class B extends A[Int](1):
  def bar: Int = foo
