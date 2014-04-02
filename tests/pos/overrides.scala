class A[T] {

  def f(x: T)(y: T = x) = y

}

class B extends A[Int] {

  def f(x: Int)(y: Int) = y

  f(2)()

}
